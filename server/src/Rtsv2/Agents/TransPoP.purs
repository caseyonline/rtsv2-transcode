module Rtsv2.Agents.TransPoP
       ( announceStreamIsAvailable
       , announceStreamStopped
       , announceTransPoPLeader
       , health
       , startLink
       , getRtts
       , routesTo
       ) where

import Prelude

import Data.Either (Either(..))
import Data.Foldable (foldM, foldl)
import Data.FoldableWithIndex (foldlWithIndex)
import Data.Maybe (Maybe(..), fromMaybe, fromMaybe')
import Data.Newtype (unwrap, wrap)
import Data.Set (Set)
import Data.Set as Set
import Data.Traversable (traverse, traverse_)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Ephemeral.Map (EMap)
import Ephemeral.Map as EMap
import Erl.Atom (Atom, atom)
import Erl.Data.List (List, index, length, nil, (:))
import Erl.Data.Map (Map)
import Erl.Data.Map as Map
import Erl.Process (spawnLink)
import Erl.Utils (Milliseconds, sleep, systemTimeMs)
import Logger (Logger)
import Logger as Logger
import Network (Network, addEdge', bestPaths, emptyNetwork, pathsBetween)
import Os (osCmd)
import Partial.Unsafe (unsafeCrashWith)
import Pinto (ServerName, StartLinkResult)
import Pinto.Gen (CallResult(..), CastResult(..))
import Pinto.Gen as Gen
import Pinto.Timer as Timer
import Prim.Row (class Nub, class Union)
import Record as Record
import Rtsv2.Names as Names
import Rtsv2.Config (IntraPoPAgentApi, ServerLocation(..), TransPoPAgentConfig)
import Rtsv2.Env as Env
import Rtsv2.Health (Health)
import Rtsv2.Health as Health
import Rtsv2.PoPDefinition (PoP)
import Rtsv2.PoPDefinition as PoPDefinition
import Serf (IpAndPort, SerfCoordinate, calcRtt)
import Serf as Serf
import Shared.Agent as Agent
import Shared.Stream (StreamId)
import Shared.Types (PoPName, ServerAddress(..))
import Shared.Utils (distinctRandomNumbers)
import SpudGun as SpudGun


type Edge = Tuple PoPName PoPName
type Rtts = Map Edge Milliseconds
type ViaPoPs = List PoPName

type State
  = { intraPoPApi :: IntraPoPAgentApi
    , currentLeader :: Maybe ServerAddress
    , weAreLeader :: Boolean
    , lastLeaderAnnouncement :: Milliseconds
    , leaderAnnouncementTimeout :: Milliseconds
    , thisNode :: ServerAddress
    , thisLocation :: ServerLocation
    , thisPoP :: PoPName
    , config :: TransPoPAgentConfig
    , serfRpcAddress :: IpAndPort
    , members :: Map PoPName (Set ServerAddress)
    , streamStateClocks :: EMap StreamId Int
    , rtts :: Rtts
    , sourceRoutes :: Map PoPName (List ViaPoPs)
    }

data StreamState = StreamAvailable
                 | StreamStopped

data TransMessage = StreamState StreamState StreamId ServerAddress

data Msg
  = LeaderTimeoutTick
  | RttRefreshTick
  | JoinAll
  | ConnectStream
  | TransPoPSerfMsg (Serf.SerfMessage TransMessage)


getRtts :: Effect Rtts
getRtts = Gen.doCall serverName
  \state@{rtts} -> pure $ CallReply rtts state


routesTo :: PoPName -> Effect (List ViaPoPs)
routesTo pop = Gen.doCall serverName
  \state@{sourceRoutes} -> pure $ CallReply (fromMaybe goDirect (Map.lookup pop sourceRoutes)) state
  where goDirect = (nil : nil) -- The list [[]] - i.e. one route that is to go via nowhere


health :: Effect Health
health =
  Gen.doCall serverName \state -> do
    currentHealth <- getHealth state
    pure $ CallReply currentHealth state
  where
    getHealth {weAreLeader: false} = pure Health.NA
    getHealth {members} = do
      allOtherPoPs <- PoPDefinition.getOtherPoPs
      pure $ Health.percentageToHealth $ (Map.size members * 100) / (length allOtherPoPs) * 100

announceStreamIsAvailable :: StreamId -> ServerAddress -> Effect Unit
announceStreamIsAvailable streamId addr =
  Gen.doCast serverName ((map CastNoReply) <<< doAnnounceStreamIsAvailable)
  where
    doAnnounceStreamIsAvailable :: State -> Effect State
    doAnnounceStreamIsAvailable state@{ weAreLeader: false } = pure state

    doAnnounceStreamIsAvailable state@{ thisLocation: (ServerLocation pop _)
                                      , serfRpcAddress
                                      } = do
      mSourcePoP <- originPoP addr
      case mSourcePoP of
        Nothing -> pure state
        Just sourcePoP
          | sourcePoP == pop -> do
            -- Message from our pop - distribute over trans-pop
            --_ <- logInfo "Local stream being delivered to trans-pop" { streamId: streamId }
            result <- Serf.event state.serfRpcAddress "streamAvailable" (StreamState StreamAvailable streamId addr) false
            _ <- maybeLogError "Trans-PoP serf event failed" result {}
            pure state
          | otherwise -> pure state

announceStreamStopped:: StreamId -> ServerAddress -> Effect Unit
announceStreamStopped streamId addr =
  Gen.doCast serverName ((map CastNoReply) <<< doAnnounceStreamStopped)
  where
    doAnnounceStreamStopped :: State -> Effect State
    doAnnounceStreamStopped state@{ weAreLeader: false } = pure state

    doAnnounceStreamStopped  state@{ thisLocation: (ServerLocation pop _)
                                   , serfRpcAddress
                                   } = do
      mSourcePoP <- originPoP addr
      case mSourcePoP of
        Nothing -> pure state
        Just sourcePoP
          | sourcePoP == pop -> do
            -- Message from our pop - distribute over trans-pop
            --_ <- logInfo "Local stream stopped being delivered to trans-pop" { streamId: streamId }
            result <- Serf.event state.serfRpcAddress "streamStopped" (StreamState StreamStopped streamId addr) false
            _ <- maybeLogError "Trans-PoP serf event failed" result {}
            pure state
          | otherwise -> pure state

announceTransPoPLeader :: ServerAddress -> Effect Unit
announceTransPoPLeader address =
  Gen.doCast serverName ((map CastNoReply) <<< doAnnounceTransPoPLeader)
  where
    doAnnounceTransPoPLeader :: State -> Effect State
    doAnnounceTransPoPLeader state@{ weAreLeader: true
                                   , thisNode
                                   , serfRpcAddress
                                   }
      | address < thisNode = do
        result <- Serf.leave serfRpcAddress
        _ <- logInfo "Another node has taken over as transpop leader; stepping down" { leader: address }
        _ <- osCmd stopScript

        now <- systemTimeMs
        pure
           $ state
              { currentLeader = Just address
              , lastLeaderAnnouncement = now
              , weAreLeader = false
              -- TODO - why is this type hint needed?  Does not compile without
              , members = Map.empty :: Map PoPName (Set ServerAddress)
              }
      | otherwise =
        pure state

    doAnnounceTransPoPLeader state@{ currentLeader
                                   , thisNode
                                   }
      | Just address /= currentLeader
          && address /= thisNode = do
        _ <- logInfo "Another node has announced as transpop leader, remember which one" { leader: address }
        now <- systemTimeMs
        pure
          $ state
              { currentLeader = Just address
              , lastLeaderAnnouncement = now
              }

    doAnnounceTransPoPLeader state = do
      now <- systemTimeMs
      pure $ state { lastLeaderAnnouncement = now }

startLink :: {config :: TransPoPAgentConfig, intraPoPApi :: IntraPoPAgentApi} -> Effect StartLinkResult
startLink args = Gen.startLink serverName (init args) handleInfo

init :: {config :: TransPoPAgentConfig, intraPoPApi :: IntraPoPAgentApi} -> Effect State
init { config: config@{ leaderTimeoutMs
                      , leaderAnnounceMs
                      , rttRefreshMs
                      , rejoinEveryMs
                      , rpcPort
                      , defaultRttMs
                      }
     , intraPoPApi} = do
  _ <- logInfo "Trans-PoP Agent Starting" {config: config}
  -- Stop any agent that might be running (in case we crashed)
  _ <- osCmd stopScript

  _ <- Gen.registerExternalMapping serverName (\m -> TransPoPSerfMsg <$> (Serf.messageMapper m))
  _ <- Timer.sendEvery serverName leaderAnnounceMs LeaderTimeoutTick

  now <- systemTimeMs

  rpcBindIp <- Env.privateInterfaceIp
  thisNode <- PoPDefinition.thisNode
  thisLocation <- PoPDefinition.thisLocation
  otherPoPNames <- PoPDefinition.getOtherPoPNames
  defaultRtts' <- getDefaultRtts config

  let
    ServerLocation thisPoP _ = thisLocation
    serfRpcAddress =
      { ip: show rpcBindIp
      , port: rpcPort
      }
    Tuple _ initialSourceRoutes = calculateRoutes Map.empty Map.empty defaultRtts' thisPoP otherPoPNames

  pure
    $ { intraPoPApi
      , config: config
      , currentLeader: Nothing
      , weAreLeader: false
      , lastLeaderAnnouncement: now
      , leaderAnnouncementTimeout: wrap leaderTimeoutMs
      , thisNode: thisNode
      , thisLocation: thisLocation
      , thisPoP: thisPoP
      , serfRpcAddress: serfRpcAddress
      , members: Map.empty
      , streamStateClocks: EMap.empty
      , rtts: defaultRtts'
      , sourceRoutes: initialSourceRoutes
    }

--------------------------------------------------------------------------------
-- Genserver callbacks
--------------------------------------------------------------------------------
handleInfo :: Msg -> State -> Effect (CastResult State)
handleInfo msg state@{ thisPoP } = case msg of
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
            _ <- logInfo "Lost connection to TransPoP Serf Agent" {}
            unsafeCrashWith ("lost_serf_connection")
          Serf.UserEvent name ltime coalesce transMessage ->
            do
              mSourcePoP <- messageOrigin transMessage
              case mSourcePoP of
                Nothing -> pure state
                Just sourcePoP
                  | sourcePoP == thisPoP -> pure state
                  | otherwise ->
                    let
                      StreamState stateChange streamId _ = transMessage
                    in
                      case shouldProcessStreamState streamId ltime state.streamStateClocks of
                        false -> do
                          _ <- logInfo "Dropping out-of-order serf message" {streamId, stateChange}
                          pure $ state

                        true -> do
                          newStreamStateClocks <- EMap.insert' streamId ltime state.streamStateClocks
                          handleTransPoPMessage transMessage (state{streamStateClocks = newStreamStateClocks})


handleTransPoPMessage :: TransMessage -> State -> Effect State
handleTransPoPMessage (StreamState StreamAvailable streamId server) state@{intraPoPApi: {announceRemoteStreamIsAvailable}} = do
  _ <- logInfo "Remote stream available" {streamId, server}
  _ <- announceRemoteStreamIsAvailable streamId server
  pure state

handleTransPoPMessage (StreamState StreamStopped streamId server) state@{intraPoPApi: {announceRemoteStreamStopped}} = do
  _ <- logInfo "Remote stream stopped" {streamId, server}
  _ <- announceRemoteStreamStopped streamId server
  pure state

--------------------------------------------------------------------------------
-- Internal functions
--------------------------------------------------------------------------------
serverName :: ServerName State Msg
serverName = Names.transPoPAgentName

messageOrigin :: TransMessage -> Effect (Maybe PoPName)
messageOrigin (StreamState _ _ addr) = originPoP addr

originPoP :: ServerAddress -> Effect (Maybe PoPName)
originPoP addr =
  do
    mServerLocation <- PoPDefinition.whereIsServer addr
    pure $ case mServerLocation of
      Nothing -> Nothing
      Just (ServerLocation sourcePoP _) -> Just sourcePoP

getDefaultRtts :: TransPoPAgentConfig -> Effect Rtts
getDefaultRtts {defaultRttMs} = do
  neighbourMap <- PoPDefinition.neighbourMap
  pure $ foldlWithIndex
    (\k acc vs ->
      foldl (\acc' a ->
              Map.insert (Tuple k a) (wrap defaultRttMs) acc')
      acc vs
    ) Map.empty neighbourMap


shouldProcessStreamState :: StreamId -> Int -> EMap StreamId Int -> Boolean
shouldProcessStreamState streamId ltime streamStateClocks =
  case EMap.lookup streamId streamStateClocks of
    Just lastLTime
      | lastLTime > ltime -> false
    _ ->
      true


membersAlive :: (List Serf.SerfMember) -> State -> Effect State
membersAlive aliveMembers state =
  do
    _ <- logInfo "Members Alive" {members: _.name <$> aliveMembers}
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
    _ <- logInfo "Members Left" {members: _.name <$> leftMembers}
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
    pure $ case mServerLocation of
      Nothing -> Nothing
      Just (ServerLocation popName _) -> Just popName

handleRttRefresh :: State -> Effect State
handleRttRefresh state@{ weAreLeader: false} =  pure state

handleRttRefresh state@{ weAreLeader: true
                       , config: config@{rttRefreshMs, defaultRttMs}
                       , members
                       , thisPoP
                       , rtts
                       } = do
    -- look up the coordinates of all members
    let allMemebers = join $ Set.toUnfoldable <$> Map.values members
        lookup addr = (Tuple addr) <$> (Serf.getCoordinate state.serfRpcAddress $ unwrap addr)
    eCoordiates <- traverse lookup allMemebers
    poPCoordinates <-
      foldM (\acc (Tuple sa eCoord) ->
              case eCoord of
                Right coord -> do
                  mPoP <- originPoP sa
                  case mPoP of
                    Nothing -> do
                      _ <- logWarning "Unknown pop for server" {misc: sa}
                      pure acc
                    Just pop -> do
                      pure $ Map.insert pop coord acc
                Left error -> do
                  _ <- logWarning "Coordinate error" {misc: {server: sa, error}}
                  pure acc
            ) Map.empty eCoordiates

    defaultRtts <- getDefaultRtts config
    otherPoPNames <- PoPDefinition.getOtherPoPNames

    let Tuple newRtts newSourceRoutes = calculateRoutes poPCoordinates rtts defaultRtts thisPoP otherPoPNames

    _ <- Timer.sendAfter serverName rttRefreshMs RttRefreshTick
    pure $ state { rtts = newRtts
                 , sourceRoutes = newSourceRoutes}


calculateRoutes :: Map PoPName SerfCoordinate -> Rtts -> Rtts -> PoPName -> List PoPName -> Tuple Rtts (Map PoPName (List ViaPoPs))
calculateRoutes poPCoordinates currentRtts defaultRtts thisPoP otherPoPNames =
    -- The new cost map should have:
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
        msToCost = unwrap >>> (*) 10
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
            _ <- state.intraPoPApi.announceTransPoPLeader
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
                       , thisNode
                       , config: {connectStreamAfterMs, rttRefreshMs}
                       , intraPoPApi: {announceTransPoPLeader: intraPoP_announceTransPoPLeader}
                       } = do
  _ <- logInfo "Leader is absent, becoming leader" {}
  _ <- osCmd startScript
  _ <- intraPoP_announceTransPoPLeader
  _ <- Timer.sendAfter serverName connectStreamAfterMs ConnectStream
  -- TODO: build default network topology map
  _ <- Timer.sendAfter serverName rttRefreshMs RttRefreshTick
  pure
    $ state
        { lastLeaderAnnouncement = now
        , weAreLeader = true
        , currentLeader = Just thisNode
        }

connectStream :: State -> Effect Unit
connectStream state@{ weAreLeader: false } = do
  pure unit

connectStream state@{serfRpcAddress} = do

  _ <- loopStreamJoin serfRpcAddress 5
  _ <- Timer.sendAfter serverName 0 JoinAll
  pure unit

  where
    loopStreamJoin rpcAddress n =
     do resp <- Serf.stream rpcAddress
        case resp of
            Left error ->
              case n of
                   0 -> do
                        _ <- logInfo "Could not connect to TransPoP Serf Agent" { error: error }
                        unsafeCrashWith ("could_not_connect_stream")
                   _ -> do
                         _ <- sleep (wrap 100)
                         loopStreamJoin serfRpcAddress $ n - 1
            Right x ->
              pure resp

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
      _ <- Timer.sendAfter serverName rejoinEveryMs JoinAll

      let
        toJoin = Map.values $ Map.difference (toMap allOtherPoPs) members :: List PoP
      if length toJoin < (length allOtherPoPs) / 2 then
        pure unit
      else
        traverse_ (\{ name, servers: serversInPoP } ->
                    do
                     indexes <- distinctRandomNumbers 1 ((length serversInPoP) - 1)
                     let
                       servers :: List ServerAddress
                       servers = traverse (\i -> index serversInPoP i) indexes
                                 # fromMaybe nil

                     (spawnFun serverAddressToSerfAddress servers)
                  )
                  toJoin
  where
  toMap :: List PoP -> Map PoPName PoP
  toMap list = foldl (\acc pop@{name} -> Map.insert name pop acc) Map.empty list

  spawnFun :: (String -> IpAndPort) -> List ServerAddress -> Effect Unit
  spawnFun addressMapper popsToJoin = void $ spawnLink (\_ -> do
                             foldl
                               ( \iAcc (ServerAddress server) -> do
                                   restResult <- SpudGun.getText ("http://" <> server <> ":3000/api/transPoPLeader")
                                   _ <- case restResult of
                                     Nothing -> pure unit
                                     Just addr -> do
                                       result <- Serf.join serfRpcAddress ((addressMapper addr) : nil) true
                                       _ <- maybeLogError "Trans-PoP serf join failed" result { server: addr }
                                       pure unit
                                   pure unit
                               )
                               mempty
                               popsToJoin
                         )

maybeLogError :: forall r b c d e. Union b (error :: e) c => Nub c d => String -> Either e r -> Record b  -> Effect Unit
maybeLogError _ (Right _) _ = pure unit
maybeLogError msg (Left err) metadata = do
  _ <- logInfo msg (Record.merge metadata {error: err})
  pure unit

logInfo :: forall a. Logger a
logInfo = doLog Logger.info

logWarning :: forall a. Logger a
logWarning = doLog Logger.warning

doLog :: forall a.  Logger {domain :: List Atom, misc :: a} -> Logger a
doLog logger msg metaData =
  logger msg { domain: (atom (show Agent.TransPoP)) : nil
             , misc: metaData }

startScript :: String
startScript = "scripts/startTransPoPAgent.sh"

stopScript :: String
stopScript = "scripts/stopTransPoPAgent.sh"
