module Rtsv2.Agents.TransPoP
       ( announceAggregatorIsAvailable
       , announceAggregatorStopped
       , handleRemoteLeaderAnnouncement
       , health
       , startLink
       , getRtts
       , getLeaderFor -- TODO - not sure this is the way forward for remote API server selection
       , routesTo
       , getNeighbours
       , getTimedRoutesTo
       , PoPRoutes
       ) where

import Prelude

import Data.Either (Either(..))
import Data.Foldable (foldM, foldl)
import Data.FoldableWithIndex (foldlWithIndex)
import Data.Int (round)
import Data.Long as Long
import Data.Maybe (Maybe(..), fromMaybe, fromMaybe', maybe)
import Data.Newtype (unwrap)
import Data.Set (Set, toUnfoldable)
import Data.Set as Set
import Data.Traversable (traverse, traverse_)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Ephemeral.Map (EMap)
import Ephemeral.Map as EMap
import Erl.Atom (Atom, atom)
import Erl.Data.Binary (Binary)
import Erl.Data.List (List, head, index, length, nil, reverse, singleton, uncons, (:))
import Erl.Data.Map (Map)
import Erl.Data.Map as Map
import Erl.Data.Tuple (tuple2)
import Erl.Process (spawnLink)
import Erl.Utils (sleep, systemTimeMs, privDir)
import Logger as Logger
import Network (Network, addEdge', bestPaths, emptyNetwork, pathsBetween)
import Os (osCmd)
import Partial.Unsafe (unsafeCrashWith)
import Pinto (ServerName, StartLinkResult)
import Pinto.Gen (CallResult(..), CastResult(..))
import Pinto.Gen as Gen
import Pinto.Timer as Timer
import PintoHelper (exposeState)
import Prim.Row (class Nub, class Union)
import Record as Record
import Rtsv2.Agents.IntraPoP (AgentClock)
import Rtsv2.Config (IntraPoPAgentApi, TransPoPAgentConfig)
import Rtsv2.Config as Config
import Rtsv2.Env as Env
import Rtsv2.Health as Health
import Rtsv2.Names as Names
import Rtsv2.PoPDefinition (PoP)
import Rtsv2.PoPDefinition as PoPDefinition
import Serf (class SerfWireMessage, IpAndPort, LamportClock, SerfCoordinate, calcRtt)
import Serf as Serf
import Shared.Common (Milliseconds(..))
import Shared.Rtsv2.Agent (SlotCharacteristics, emptySlotCharacteristics)
import Shared.Rtsv2.Agent.State as PublicState
import Shared.Rtsv2.JsonLd as JsonLd
import Shared.Rtsv2.Router.Endpoint.System as System
import Shared.Rtsv2.Stream (AgentKey)
import Shared.Rtsv2.Types (CanaryState(..), Health(..), PoPName, Server, ServerAddress(..), extractAddress, extractPoP)
import Shared.Utils (distinctRandomNumbers)
import SpudGun (bodyToString)
import SpudGun as SpudGun

foreign import to_binary :: forall a. a -> Binary

type Edge = Tuple PoPName PoPName
type Rtts = Map Edge Milliseconds
type ViaPoPs = List PoPName

type PoPRoutes = List (List PoPName)

type StartArgs =
  { config :: TransPoPAgentConfig
  , intraPoPApi :: IntraPoPAgentApi
  , canaryState :: CanaryState
  }

type LamportClocks =
  { aggregatorClocks :: AgentClock
  }

type State
  = { intraPoPApi :: IntraPoPAgentApi
    , canary :: CanaryState
    , currentLeader :: Maybe Server
    , weAreLeader :: Boolean
    , lastLeaderAnnouncement :: Milliseconds
    , leaderAnnouncementTimeout :: Milliseconds
    , thisServer :: Server
    , config :: TransPoPAgentConfig
    , healthConfig :: Config.HealthConfig
    , serfRpcAddress :: IpAndPort
    , members :: Map PoPName (Set ServerAddress)
    , agentClocks :: LamportClocks
    , rtts :: Rtts
    , sourceRoutes :: Map PoPName PoPRoutes
    }

data EventType
  = Available
  | Stopped

data TransMessage
  = TMAggregatorState EventType AgentKey ServerAddress SlotCharacteristics

instance serfWireMessageTM :: SerfWireMessage TransMessage where
  toWireMessage payload@(TMAggregatorState Available agentKey serverAddress slotCharacteristics) =
    tuple2 "streamAvailable" $ to_binary payload
  toWireMessage payload@(TMAggregatorState Stopped agentKey serverAddr slotCharacteristics) =
    tuple2 "streamStopped" $ to_binary payload



data Msg
  = LeaderTimeoutTick
  | RttRefreshTick
  | JoinAll
  | ConnectStream
  | TransPoPSerfMsg (Serf.SerfMessage TransMessage)


getNeighbours :: Effect (Tuple Server (List (JsonLd.TimedRouteNeighbour List)))
getNeighbours = exposeState doGetNeighbours serverName
  where
    doGetNeighbours {thisServer, members} =
      Tuple thisServer $ (\(Tuple pop servers) -> {pop, servers: toUnfoldable servers}) <$> Map.toUnfoldable members

getTimedRoutesTo :: PoPName -> Effect (PublicState.TimedPoPRoutes List)
getTimedRoutesTo pop = exposeState doGetTimedRoutes serverName
  where
    getRtt rttMap from to = fromMaybe (Milliseconds $ Long.fromInt 1000) $ Map.lookup (Tuple from to) rttMap
    augmentWithRtt :: Rtts -> PoPName -> PoPName -> PublicState.TimedPoPStep
    augmentWithRtt rttMap from to = {from, to, rtt: round $ Long.toNumber $ unwrap $ getRtt rttMap from to}
    augmentWithRtts state@{rtts: rttMap}  acc from to vias =
      case uncons vias of
        Nothing -> reverse $ (augmentWithRtt rttMap from to) : acc
        Just {head, tail} ->
          augmentWithRtts state (augmentWithRtt rttMap from head : acc) head to tail
    doGetTimedRoutes state@{thisServer, sourceRoutes} =
      let
        thisPoP = extractPoP thisServer
        routes = (augmentWithRtts state nil thisPoP pop) <$> (fromMaybe nil $ Map.lookup pop sourceRoutes)
       in
      { from: thisPoP
      , to: pop
      , routes
      }



getRtts :: Effect Rtts
getRtts = exposeState _.rtts serverName

getLeaderFor :: PoPName -> Effect (Maybe ServerAddress)
getLeaderFor pop =
  exposeState (\state -> head <<< Set.toUnfoldable =<< Map.lookup pop state.members) serverName


routesTo :: PoPName -> Effect PoPRoutes
routesTo pop = Gen.doCall serverName
  \state@{sourceRoutes, thisServer} -> do
    resp <-
      if extractPoP thisServer == pop
      then pure nil -- source and desitination are the same!
      else
        case Map.lookup pop sourceRoutes of
          Nothing -> do
            -- We could not find a route to the pop
            -- just have a single, direct route [[pop]]
            logWarning "No route returned for" {pop}
            pure $ singleton $ singleton pop
          Just viaLists ->
            -- the map contains just the the via pops
            -- it is convenient to have the final destination added to them...
            pure $ (_ <> singleton pop) <$> viaLists
    pure $ CallReply resp state


health :: Effect Health
health =
  Gen.doCall serverName \state -> do
    currentHealth <- getHealth state
    pure $ CallReply currentHealth state
  where
    getHealth {weAreLeader: false} = pure NA
    getHealth {members, healthConfig} = do
      allOtherPoPs <- PoPDefinition.getOtherPoPs
      pure $ Health.percentageToHealth healthConfig $ (Map.size members) * 100 / ((Map.size allOtherPoPs) + 1)

announceAggregatorIsAvailable :: SlotCharacteristics -> AgentKey -> Server -> Effect Unit
announceAggregatorIsAvailable slotCharacteristics agentKey server =
  Gen.doCast serverName ((map CastNoReply) <<< doAnnounceStreamIsAvailable)
  where
    doAnnounceStreamIsAvailable :: State -> Effect State
    doAnnounceStreamIsAvailable state@{ weAreLeader: false } = pure state

    doAnnounceStreamIsAvailable state@{ thisServer
                                      , serfRpcAddress
                                      } = do
      sendToTransSerfNetwork state $ TMAggregatorState Available agentKey (extractAddress server) slotCharacteristics
      pure state

announceAggregatorStopped :: AgentKey -> Server -> Effect Unit
announceAggregatorStopped agentKey server =
  Gen.doCast serverName ((map CastNoReply) <<< doAnnounceStreamStopped)
  where
    doAnnounceStreamStopped :: State -> Effect State
    doAnnounceStreamStopped state@{ weAreLeader: false } = pure state

    doAnnounceStreamStopped  state@{ thisServer
                                   , serfRpcAddress
                                   } = do
      -- Don't think this test is required - surely it can only be from this pop...
      if extractPoP server == extractPoP thisServer
      then do
            -- Message from our pop - distribute over trans-pop
            --logInfo "Local stream stopped being delivered to trans-pop" { slotId: slotId }
            sendToTransSerfNetwork state $ TMAggregatorState Stopped agentKey (extractAddress server) emptySlotCharacteristics
            pure state
      else pure state

handleRemoteLeaderAnnouncement :: Server -> Effect Unit
handleRemoteLeaderAnnouncement server =
  Gen.doCast serverName ((map CastNoReply) <<< doHandleRemoteLeaderAnnouncement)
  where
    doHandleRemoteLeaderAnnouncement :: State -> Effect State
    doHandleRemoteLeaderAnnouncement state@{ weAreLeader: true
                                           , thisServer
                                           , serfRpcAddress
                                           }
      | extractAddress server < extractAddress thisServer = do
        logInfo "Another node has taken over as transpop leader; stepping down" { leader: server }
        result <- Serf.leave serfRpcAddress
        void $ osCmd stopScript

        now <- systemTimeMs
        pure
           $ state
              { currentLeader = Just server
              , lastLeaderAnnouncement = now
              , weAreLeader = false
              -- TODO - why is this type hint needed?  Does not compile without
              , members = Map.empty :: Map PoPName (Set ServerAddress)
              }
      | otherwise =
        pure state

    doHandleRemoteLeaderAnnouncement state@{ currentLeader
                                           , thisServer
                                           }
      | Just server /= currentLeader
          && extractAddress server /= extractAddress thisServer = do
        logInfo "Another node has announced as transpop leader, remember which one" { leader: server }
        now <- systemTimeMs
        pure
          $ state
              { currentLeader = Just server
              , lastLeaderAnnouncement = now
              }

    doHandleRemoteLeaderAnnouncement state = do
      now <- systemTimeMs
      pure $ state { lastLeaderAnnouncement = now }

startLink :: StartArgs -> Effect StartLinkResult
startLink args = Gen.startLink serverName (init args) handleInfo

init :: StartArgs -> Effect State
init { config: config@{ leaderTimeoutMs
                      , leaderAnnounceMs
                      , rttRefreshMs
                      , rejoinEveryMs
                      , rpcPort
                      , defaultRttMs
                      }
     , intraPoPApi
     , canaryState} = do
  logInfo "Trans-PoP Agent Starting" {config: config}
  healthConfig <- Config.healthConfig
  -- Stop any agent that might be running (in case we crashed)
  void $ osCmd stopScript

  Gen.registerExternalMapping serverName (\m -> TransPoPSerfMsg <$> (Serf.messageMapper m))
  void $ Timer.sendEvery serverName leaderAnnounceMs LeaderTimeoutTick

  now <- systemTimeMs

  rpcBindIp <- Env.transSerfIp
  thisServer <- PoPDefinition.getThisServer
  otherPoPNames <- PoPDefinition.getOtherPoPNames
  defaultRtts' <- getDefaultRtts config

  let
    thisPoP = extractPoP thisServer
    serfRpcAddress =
      { ip: show rpcBindIp
      , port: rpcPort
      }
    Tuple _ initialSourceRoutes = calculateRoutes Map.empty Map.empty defaultRtts' thisPoP otherPoPNames

  pure
    $ { intraPoPApi
      , canary: canaryState
      , config
      , healthConfig
      , currentLeader: Nothing
      , weAreLeader: false
      , lastLeaderAnnouncement: now
      , leaderAnnouncementTimeout: Milliseconds $ Long.fromInt leaderTimeoutMs
      , thisServer: thisServer
      , serfRpcAddress: serfRpcAddress
      , members: Map.empty
      , agentClocks: { aggregatorClocks: Map.empty
                     }
      , rtts: defaultRtts'
      , sourceRoutes: initialSourceRoutes
    }

--------------------------------------------------------------------------------
-- Genserver callbacks
--------------------------------------------------------------------------------
handleInfo :: Msg -> State -> Effect (CastResult State)
handleInfo msg state =
 case msg of
  LeaderTimeoutTick -> CastNoReply <$> handleTick state

  RttRefreshTick ->
    CastNoReply <$> handleRttRefresh state

  JoinAll ->
    CastNoReply state <$ joinAllSerf state

  ConnectStream ->
    CastNoReply state <$ connectStream state

  TransPoPSerfMsg tmsg ->
    CastNoReply <$>
        case tmsg of
          Serf.MemberAlive members -> membersAlive members state
          Serf.MemberLeaving -> pure state
          Serf.MemberLeft members -> membersLeft members state
          Serf.MemberFailed -> pure state
          Serf.StreamFailed -> do
            logInfo "Lost connection to TransPoP Serf Agent" {}
            unsafeCrashWith ("lost_serf_connection")
          Serf.UserEvent name ltime coalesce transMessage ->
            case transMessage of
              TMAggregatorState eventType slotId msgOrigin slotCharacteristics -> do
                handleAgentMessage ltime eventType slotId msgOrigin slotCharacteristics state



handleTransPoPMessage :: TransMessage -> State -> Effect State
handleTransPoPMessage (TMAggregatorState Available agentKey address slotCharacteristics) state@{intraPoPApi} = do
  logInfo "Remote stream available" {agentKey, address}
  mServer <- PoPDefinition.whereIsServer address
  case mServer of
    Nothing -> pure state
    Just server -> do
      intraPoPApi.announceOtherPoPAggregatorIsAvailable slotCharacteristics agentKey server
      pure state

handleTransPoPMessage (TMAggregatorState Stopped agentKey address slotCharacteristics) state@{intraPoPApi} = do
  logInfo "Remote stream stopped" {agentKey, address}
  mServer <- PoPDefinition.whereIsServer address
  case mServer of
    Nothing -> pure state
    Just server -> do
      intraPoPApi.announceOtherPoPAggregatorStopped agentKey server
      pure state

--------------------------------------------------------------------------------
-- Internal functions
--------------------------------------------------------------------------------
serverName :: ServerName State Msg
serverName = Names.transPoPName

originPoP :: ServerAddress -> Effect (Maybe PoPName)
originPoP addr =
  do
    (map extractPoP) <$> PoPDefinition.whereIsServer addr

getDefaultRtts :: TransPoPAgentConfig -> Effect Rtts
getDefaultRtts {defaultRttMs} = do
  neighbourMap <- PoPDefinition.neighbourMap
  pure $ foldlWithIndex
    (\k acc vs ->
      foldl (\acc' a ->
              Map.insert (Tuple k a) (Milliseconds $ Long.fromInt defaultRttMs) acc')
      acc vs
    ) Map.empty neighbourMap


shouldProcessStreamState :: AgentKey -> LamportClock -> EMap AgentKey LamportClock -> Boolean
shouldProcessStreamState slotId ltime streamStateClocks =
  case EMap.lookup slotId streamStateClocks of
    Just lastLTime
      | lastLTime > ltime -> false
    _ ->
      true


membersAlive :: (List Serf.SerfMember) -> State -> Effect State
membersAlive aliveMembers state =
  do
    logInfo "Members Alive" {members: _.name <$> aliveMembers}
    newMembers <- foldl addPoP (pure $ state.members) aliveMembers
    pure state {members = newMembers}
  where
    addPoP :: Effect (Map PoPName (Set ServerAddress)) -> Serf.SerfMember -> Effect (Map PoPName (Set ServerAddress))
    addPoP eMembers aliveMember@{name: aliveName} =
      do
        members <- eMembers
        mPoP <- getPoP aliveMember
        pure $ fromMaybe members ((\popName ->
                                    let
                                      existingMembers = case Map.lookup popName members of
                                                          Just existing -> existing
                                                          Nothing -> Set.empty
                                      newMembers = Set.insert (ServerAddress aliveName) existingMembers
                                    in
                                     Map.insert popName newMembers members
                                  ) <$> mPoP)

membersLeft :: (List Serf.SerfMember) -> State -> Effect State
membersLeft leftMembers state =
  do
    logInfo "Members Left" {members: _.name <$> leftMembers}
    newMembers <- foldl removePoP (pure state.members) leftMembers
    pure state {members = newMembers}
  where
    removePoP :: Effect (Map PoPName (Set ServerAddress)) -> Serf.SerfMember -> Effect (Map PoPName (Set ServerAddress))
    removePoP eMembers leftMember@{name: leftName} =
      do
        members <- eMembers
        mPoP <- getPoP leftMember
        pure $ fromMaybe members ((\popName ->
                                    let
                                      existingMembers = case Map.lookup popName members of
                                                          Just existing -> existing
                                                          Nothing -> Set.empty
                                      newMembers = Set.delete (ServerAddress leftName) existingMembers
                                    in
                                     Map.insert popName newMembers members
                                  ) <$> mPoP)

getPoP :: Serf.SerfMember -> Effect (Maybe PoPName)
getPoP {name : memberName} =
  do
    mServerLocation <- PoPDefinition.whereIsServer (ServerAddress memberName)
    pure $ extractPoP <$> mServerLocation

handleRttRefresh :: State -> Effect State
handleRttRefresh state@{ weAreLeader: false} =  pure state

handleRttRefresh state@{ weAreLeader: true
                       , config: config@{rttRefreshMs, defaultRttMs}
                       , members
                       , thisServer
                       , rtts
                       } = do
    -- look up the coordinates of all members
    let allMembers = join $ Set.toUnfoldable <$> Map.values members
        lookup addr = (Tuple addr) <$> (Serf.getCoordinate state.serfRpcAddress $ unwrap addr)
    eCoordinates <- traverse lookup allMembers
    poPCoordinates <-
      foldM (\acc (Tuple sa eCoord) ->
              case eCoord of
                Right coord -> do
                  mPoP <- originPoP sa
                  case mPoP of
                    Nothing -> do
                      logWarning "Unknown pop for server" {misc: sa}
                      pure acc
                    Just pop -> do
                      pure $ Map.insert pop coord acc
                Left error -> do
                  logWarning "Coordinate error" {misc: {server: sa, error}}
                  pure acc
            ) Map.empty eCoordinates
    defaultRtts <- getDefaultRtts config
    otherPoPNames <- PoPDefinition.getOtherPoPNames

    let
      thisPoP = extractPoP thisServer
      Tuple newRtts newSourceRoutes = calculateRoutes poPCoordinates rtts defaultRtts thisPoP otherPoPNames

    void $ Timer.sendAfter serverName rttRefreshMs RttRefreshTick
    pure $ state { rtts = newRtts
                 , sourceRoutes = newSourceRoutes}


calculateRoutes :: Map PoPName SerfCoordinate -> Rtts -> Rtts -> PoPName -> List PoPName -> Tuple Rtts (Map PoPName (List ViaPoPs))
calculateRoutes poPCoordinates currentRtts defaultRtts thisPoP otherPoPNames =
    -- The new rtts map should have:
    -- * only those edges now in the latest PoPDefinition
    -- With:
    --  * Newly measured RTTs (from the new coordinates)
    --  * Previous rtts (from current map)
    --  * Default rtt
    let bestGuessRtt edge@(Tuple fromPoP toPoP) acc def =
          let maybeBest :: Maybe Milliseconds
              maybeBest = calcRtt <$> Map.lookup fromPoP poPCoordinates <*> Map.lookup toPoP poPCoordinates
              guess = fromMaybe' (\_ -> fromMaybe def $ Map.lookup edge currentRtts) maybeBest
          in  Map.insert edge guess acc
        newRtts = foldlWithIndex bestGuessRtt Map.empty defaultRtts
        msToCost :: Milliseconds -> Int
        msToCost = unwrap >>> Long.toNumber >>> round >>> (*) 10
        newNetwork :: Network PoPName
        newNetwork = foldlWithIndex (\(Tuple from to) acc ms -> addEdge' from to (msToCost ms) acc) emptyNetwork newRtts
        -- Get the bestroute pairs to all the other pops
        sourceRoutes = foldl (\acc popName ->
                               case bestPaths $ pathsBetween newNetwork thisPoP popName of
                                 Just {path1, path2} -> Map.insert popName (_.via <$> (path1 : path2 : nil)) acc
                                 Nothing -> acc
                             ) Map.empty otherPoPNames
    in Tuple newRtts sourceRoutes


handleTick :: State -> Effect State
handleTick = case _ of
  state@{ weAreLeader: true } -> do
            state.intraPoPApi.announceTransPoPLeader
            pure state

  state -> do
      now <- systemTimeMs
      if (state.lastLeaderAnnouncement + state.leaderAnnouncementTimeout) < now
        then becomeLeader now state
        else pure $ state


becomeLeader :: Milliseconds -> State -> Effect State
becomeLeader now state@{ lastLeaderAnnouncement
                       , leaderAnnouncementTimeout
                       , serfRpcAddress
                       , thisServer
                       , config: {connectStreamAfterMs, rttRefreshMs}
                       , intraPoPApi: {announceTransPoPLeader: intraPoP_announceTransPoPLeader}
                       } = do
  logInfo "Leader is absent, becoming leader" {}
  void $ osCmd startScript
  intraPoP_announceTransPoPLeader
  void $ Timer.sendAfter serverName connectStreamAfterMs ConnectStream
  void $ Timer.sendAfter serverName rttRefreshMs RttRefreshTick
  pure
    $ state
        { lastLeaderAnnouncement = now
        , weAreLeader = true
        , currentLeader = Just thisServer
        }

connectStream :: State -> Effect Unit
connectStream state@{ weAreLeader: false } = do
  pure unit

connectStream state@{serfRpcAddress} = do

  loopStreamJoin serfRpcAddress 5
  void $ Timer.sendAfter serverName 0 JoinAll
  pure unit

  where
    loopStreamJoin rpcAddress n =
     do resp <- Serf.stream rpcAddress
        case resp of
            Left error ->
              case n of
                   0 -> do
                        logInfo "Could not connect to TransPoP Serf Agent" { error: error }
                        unsafeCrashWith ("could_not_connect_stream")
                   _ -> do
                         sleep $ Milliseconds $ Long.fromInt 100
                         loopStreamJoin serfRpcAddress $ n - 1
            Right x ->
              pure unit

joinAllSerf :: State -> Effect Unit
joinAllSerf state@{ weAreLeader: false } = do
  pure unit

joinAllSerf state@{ config: config@{rejoinEveryMs}, serfRpcAddress, members } =
  let
    serverAddressToSerfAddress :: String -> IpAndPort
    serverAddressToSerfAddress s =
      { ip: s
      , port: config.bindPort
      }
  in
    do
      allOtherPoPs <- PoPDefinition.getOtherPoPs
      void $ Timer.sendAfter serverName rejoinEveryMs JoinAll

      let
        toJoin = Map.values $ Map.difference allOtherPoPs members :: List PoP
      if length toJoin < (Map.size allOtherPoPs) / 2 then
        pure unit
      else
        traverse_ (\{ name, servers: serversInPoP } ->
                    do
                     indexes <- distinctRandomNumbers 1 ((length serversInPoP) - 1)
                     let
                       servers :: List ServerAddress
                       servers = traverse (\i -> (unwrap >>> _.address) <$> index serversInPoP i) indexes
                                 # fromMaybe nil
                     (spawnFun serverAddressToSerfAddress servers)
                  )
                  toJoin
  where
  spawnFun :: (String -> IpAndPort) -> List ServerAddress -> Effect Unit
  spawnFun addressMapper popsToJoin = void $ spawnLink (\_ -> do
                             foldl
                               ( \iAcc serverAddress  -> do
                                   url <- System.makeUrlAddr serverAddress System.TransPoPLeaderE
                                   restResult <- bodyToString <$> SpudGun.getText url
                                   case restResult of
                                     Left _ -> pure unit
                                     Right "" -> pure unit
                                     Right addr -> do
                                       result <- Serf.join serfRpcAddress ((addressMapper addr) : nil) config.replayMessagesOnJoin
                                       maybeLogError "Trans-PoP serf join failed" result { server: addr }
                               )
                               mempty
                               popsToJoin
                         )




handleAgentMessage :: LamportClock -> EventType -> AgentKey -> ServerAddress -> SlotCharacteristics -> State -> Effect State
handleAgentMessage msgLTime eventType agentKey msgServerAddress slotCharacteristics
                   state@{thisServer} = do
  -- let _ = spy "agentMessage" {name: agentMessageHandler.name, eventType, slotId, msgServerAddress}
  -- Make sure the message is from a known origin and does not have an expired Lamport clock
  if msgServerAddress == extractAddress thisServer
  then
    pure state
  else let agentClock = state.agentClocks.aggregatorClocks in
    if Map.lookup (Tuple msgServerAddress agentKey) agentClock # maybe false (_ >= msgLTime)
    then
      pure state
    else do
      -- TODO - maybe cache the db for a while to prevent hot calls, or use ETS etc
      mServer <- PoPDefinition.whereIsServer msgServerAddress
      case mServer of
        Nothing -> do
          logWarning "message from unknown server" {msgServerAddress}
          pure state
        Just msgServer
          -- Opposite logic to IntraPoP - only forward things that are NOT from this pop
          | extractPoP msgServer == extractPoP state.thisServer ->
            pure state
          | otherwise -> do
            let
              newAgentClock = Map.insert (Tuple msgServerAddress agentKey) msgLTime agentClock

            case eventType of
              Available -> do
                state.intraPoPApi.announceOtherPoPAggregatorIsAvailable slotCharacteristics agentKey msgServer
              Stopped -> do
                state.intraPoPApi.announceOtherPoPAggregatorStopped agentKey msgServer
            pure $ state { agentClocks = {aggregatorClocks : newAgentClock} }

startScript :: String
startScript =  privDir(atom "rtsv2") <> "/scripts/startTransPoPAgent.sh"

stopScript :: String
stopScript = privDir(atom "rtsv2") <> "/scripts/stopTransPoPAgent.sh"

sendToTransSerfNetwork :: State -> TransMessage -> Effect Unit
sendToTransSerfNetwork {canary: Canary}  _msg = pure unit
sendToTransSerfNetwork state msg = do
  result <- Serf.event state.serfRpcAddress msg false
  maybeLogError "Trans-PoP serf event failed" result {msg}

--------------------------------------------------------------------------------
-- Log helpers
--------------------------------------------------------------------------------
domain :: List Atom
domain = serverName # Names.toDomain # singleton

logInfo :: forall report. String -> { | report } -> Effect Unit
logInfo = Logger.info <<< Logger.traceMetadata domain

logWarning :: forall report. String -> { | report } -> Effect Unit
logWarning = Logger.warning <<< Logger.traceMetadata domain

maybeLogError :: forall a b c d e. Union b (error :: e) c => Nub c d => String -> Either e a -> Record b -> Effect Unit
maybeLogError _ (Right _) _ = pure unit
maybeLogError msg (Left err) metadata = do
  logInfo msg (Record.merge metadata {error: err})
  pure unit
