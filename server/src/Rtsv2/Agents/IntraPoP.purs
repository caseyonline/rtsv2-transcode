module Rtsv2.Agents.IntraPoP
  ( startLink
  , isStreamIngestAvailable
  , isIngestActive

  , announceLoad

  , announceTransPoPLeader

  , announceEgestIsAvailable
  , announceEgestStopped
  , announceStreamIsAvailable
  , announceStreamStopped

  , announceStreamRelayIsAvailable

  , announceRemoteStreamIsAvailable
  , announceRemoteStreamStopped

    -- TODO - delete me
    -- if out of capacity pick a random pop - ask if it has capacity
  , announceRemoteEgestIsAvailable

  , getPublicState

  , whereIsIngestAggregator
  , whereIsStreamRelay
  , whereIsEgest
  , getIdleServer
  , currentTransPoPLeader
  , launchLocalOrRemoteGeneric
  , health
  , bus
  , IntraMessage(..)
  , StreamState(..)
  , EgestState(..)
  , IntraPoPBusMessage(..)
  ) where


import Prelude

import Bus as Bus
import Data.Either (Either(..), hush)
import Data.Filterable (filterMap)
import Data.Foldable (foldM, foldl)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (wrap)
import Data.Set (Set)
import Data.Set as Set
import Data.Traversable (traverse_)
import Data.Tuple (Tuple(..), fst)
import Effect (Effect)
import Effect.Random (randomInt)
import Ephemeral.Map (EMap)
import Ephemeral.Map as EMap
import Ephemeral.MultiMap (EMultiMap)
import Ephemeral.MultiMap as MultiMap
import Erl.Atom (Atom)
import Erl.Data.List (List, index, length, nil, singleton, sortBy, take, uncons)
import Erl.Data.Map (Map, alter, fromFoldable, values)
import Erl.Data.Map as Map
import Erl.Process (Process, spawnLink)
import Erl.Utils (Milliseconds, Ref, makeRef)
import Erl.Utils as Erl
import Logger (Logger, spy)
import Logger as Logger
import Partial.Unsafe (unsafeCrashWith)
import Pinto (ServerName, StartLinkResult)
import Pinto.Gen (CallResult(..), CastResult(..))
import Pinto.Gen as Gen
import Pinto.Timer as Timer
import PintoHelper (exposeState)
import Prim.Row (class Nub, class Union)
import Record as Record
import Rtsv2.Agents.Locator.Types (LocalOrRemote(..), NoCapacity(..), ResourceResp, ServerSelectionPredicate)
import Rtsv2.Config as Config
import Rtsv2.Env as Env
import Rtsv2.Health (Health, percentageToHealth)
import Rtsv2.Names as Names
import Rtsv2.PoPDefinition as PoPDefinition
import Serf (IpAndPort, LamportClock)
import Serf as Serf
import Shared.Stream (StreamId(..), StreamAndVariant(..))
import Shared.Types (Load, Server(..), ServerAddress(..), ServerLoad(..), extractAddress, serverLoadToServer, toServer, toServerLoad)
import Shared.Types.Agent.State as PublicState


announceStreamRelayIsAvailable = pure unit


type ScreenedMessage
  = { clockState :: Map ServerAddress LamportClock
    , from :: Server
--    , ref :: Ref
    }

type MemberInfo = { serfMember :: Serf.SerfMember
                  , load :: Load
                  , server :: Server
                  }

type LamportClocks =
  { streamStateClocks :: Map ServerAddress LamportClock
  , egestStateClocks :: Map ServerAddress LamportClock
  , loadClocks :: Map ServerAddress LamportClock
  , popLeaderClocks :: Map ServerAddress LamportClock
  , livenessClocks :: Map ServerAddress LamportClock
  }

type AggregatorLocations = Map StreamId Server

type State
  = { transPoPApi :: Config.TransPoPAgentApi
    , config :: Config.IntraPoPAgentConfig
    , serfRpcAddress :: IpAndPort
    , currentTransPoPLeader :: Maybe Server
    , egestLocations :: EMultiMap StreamId Server
    , streamRelayLocations :: EMap StreamId Server

    , aggregatorLocations :: AggregatorLocations
    , remoteAggregatorLocations :: Map Server (Set StreamId)

    , thisServerRef :: Ref
    , serverRefs :: EMap Server Ref

    , thisServer :: Server
    , load :: Load
    , members :: Map ServerAddress MemberInfo
    , expireThreshold :: Milliseconds
    , lamportClocks :: LamportClocks
    }

data StreamState = StreamAvailable
                 | StreamStopped

data EgestState = EgestAvailable
                | EgestStopped

data IntraMessage = IMStreamState StreamState StreamId ServerAddress
                  | IMEgestState EgestState StreamId ServerAddress
                  | IMServerLoad ServerAddress Load
                  | IMTransPoPLeader ServerAddress

                  | IMLiveness ServerAddress Ref

data Msg
  = JoinAll
  | GarbageCollect
  | IntraPoPSerfMsg (Serf.SerfMessage IntraMessage)
  | Liveness

data IntraPoPBusMessage =
  IngestAggregatorExited StreamId Server



--------------------------------------------------------------------------------
-- API
--------------------------------------------------------------------------------
health :: Effect Health
health =
  Gen.doCall serverName \state@{ members } -> do
    allOtherServers <- PoPDefinition.getOtherServersForThisPoP
    let
      currentHealth = percentageToHealth $ (Map.size members) / ((length allOtherServers) + 1) * 100
    pure $ CallReply currentHealth state

bus :: Bus.Bus IntraPoPBusMessage
bus = Bus.bus "intrapop_bus"



getPublicState :: Effect PublicState.IntraPoP
getPublicState = exposeState publicState serverName
  where
    toFoo (Tuple k v) = {streamId: k
                         , server: v}
    publicState state =
      { aggregatorLocations: toFoo <$>  Map.toUnfoldable state.aggregatorLocations
      }

isStreamIngestAvailable :: StreamId -> Effect Boolean
isStreamIngestAvailable streamId =
  Gen.call serverName \state@{ aggregatorLocations } ->
    CallReply (Map.member streamId aggregatorLocations) state

isIngestActive :: StreamAndVariant -> Effect Boolean
isIngestActive (StreamAndVariant s v) = Gen.call serverName \state -> CallReply false state

whereIsIngestAggregator :: StreamId -> Effect (Maybe Server)
whereIsIngestAggregator (StreamId streamId) =
  Gen.call serverName \state ->
    CallReply (Map.lookup (StreamId streamId) state.aggregatorLocations) state

whereIsStreamRelay :: StreamId -> Effect (Maybe (LocalOrRemote Server))
whereIsStreamRelay streamId =
  exposeState lookupSR serverName
  where
    lookupSR state =
      let mRelay = EMap.lookup streamId state.streamRelayLocations
      in
       (\rel -> if rel == state.thisServer then Local rel else Remote rel) <$>  mRelay

whereIsEgest :: StreamId -> Effect (List ServerLoad)
whereIsEgest streamId =
  Gen.call serverName
  \state@{egestLocations, members} ->
    let withLoad (Server el) = serverLoad <$> Map.lookup el.address members
    in
    CallReply (filterMap withLoad $ MultiMap.lookup streamId state.egestLocations) state

--------------------------------------------------------------------------------
-- TODO re-read Power of Two articles such as
-- https://www.nginx.com/blog/nginx-power-of-two-choices-load-balancing-algorithm/
-- TODO - predicate should probable be ServerLoad -> Weighting
--------------------------------------------------------------------------------
getIdleServer :: ServerSelectionPredicate -> Effect (ResourceResp ServerLoad)
getIdleServer pred = Gen.doCall serverName
  (\state@{thisServer, members, load} -> do
      let thisServerLoad = toServerLoad thisServer load
      if pred thisServerLoad
      then pure $ CallReply (Right $ Local thisServerLoad) state
      else
        let n = 2
            nMostIdleWithCapacity =
              values members
              # filterMap (\memberInfo ->
                            let serverWithLoad = serverLoad memberInfo
                            in if pred serverWithLoad then Just serverWithLoad else Nothing)
              # sortBy (\(ServerLoad sl1) (ServerLoad sl2) -> compare sl1.load sl2.load)
              # take n
            numServers = length nMostIdleWithCapacity
        in do
          resp <-
            case numServers of
              0 ->
                pure $ Left NoCapacity
              1 ->
                case uncons nMostIdleWithCapacity of
                  Just {head} -> pure $ Right $ Remote head
                  Nothing -> pure $ Left NoCapacity
              _ ->
                do
                  -- TODO - always seems to give the same answer!
                  chosenIndex <- randomInt 0 (numServers - 1)
                  pure $ maybe (Left NoCapacity) (Right <<< Remote) $ index nMostIdleWithCapacity chosenIndex
          pure $ CallReply resp state
  )


currentTransPoPLeader :: Effect (Maybe Server)
currentTransPoPLeader =
  Gen.call serverName
    ( \state@{ currentTransPoPLeader: value } ->
        CallReply value state
    )

-- Called by Load to indicate load on this node
announceLoad :: Load -> Effect Unit
announceLoad load =
  Gen.doCast serverName
    \state@{ thisServer, members } -> do
      let
        thisNodeAddress = extractAddress thisServer
        newMembers = alter (map (\ memberInfo -> memberInfo { load = load })) thisNodeAddress members
      _ <- sendToIntraSerfNetwork state "loadUpdate" (IMServerLoad thisNodeAddress load)
      pure $ Gen.CastNoReply state { members = newMembers , load = load}

-- Called by EgestAgent to indicate egest on this node
announceEgestIsAvailable :: StreamId -> Effect Unit
announceEgestIsAvailable streamId =
  Gen.doCast serverName
    \state@{ egestLocations, thisServer } -> do
      newEgestLocations <- MultiMap.insert' streamId thisServer egestLocations
      logInfo "Local egest available" { streamId: streamId }
      _ <- sendToIntraSerfNetwork state "egestAvailable" (IMEgestState EgestAvailable streamId $ extractAddress thisServer)
      pure $ Gen.CastNoReply state { egestLocations = newEgestLocations }

-- Called by EgestAgent to indicate egest on this node has stopped
announceEgestStopped :: StreamId -> Effect Unit
announceEgestStopped streamId =
  Gen.doCast serverName
    \state@{ egestLocations, thisServer } -> do
      let
        newEgestLocations = MultiMap.delete streamId thisServer egestLocations
      logInfo "Local egest stopped" {streamId}
      _ <- sendToIntraSerfNetwork state "egestStopped" $ IMEgestState EgestStopped streamId $ extractAddress thisServer
      pure $ Gen.CastNoReply state { egestLocations = newEgestLocations }

-- Called when a client here result in a remote egest being created
announceRemoteEgestIsAvailable :: StreamId -> Server -> Effect Unit
announceRemoteEgestIsAvailable streamId server =
  Gen.doCast serverName
    \state -> do
      logInfo "New remote egest is avaiable due to a local client" {streamId, server}
      Gen.CastNoReply <$> insertEgest streamId server state


-- Called by IngestAggregator to indicate stream on this node
announceStreamIsAvailable :: StreamId -> Effect Unit
announceStreamIsAvailable streamId =
  Gen.doCast serverName
    \state@{ aggregatorLocations, thisServer, transPoPApi:{announceStreamIsAvailable: transPoP_announceStreamIsAvailable} } -> do
      logInfo "New local stream is available" {streamId}
      _ <- sendToIntraSerfNetwork state "streamAvailable" (IMStreamState StreamAvailable streamId (extractAddress thisServer))
      _ <- transPoP_announceStreamIsAvailable streamId thisServer

      pure $ Gen.CastNoReply $ insertAggregator streamId thisServer state

insertEgest :: StreamId -> Server -> State -> Effect State
insertEgest streamId server state@{ egestLocations } =
  do
    newEgestLocations <- MultiMap.insert' streamId server egestLocations
    pure $ state { egestLocations = newEgestLocations }


insertRemoteAggregator :: StreamId -> Server -> State -> State
insertRemoteAggregator streamId server state@{ remoteAggregatorLocations } =
  let
    currentStreamsOnRemoteServer = fromMaybe Set.empty (Map.lookup server remoteAggregatorLocations)
    newStreams = Set.insert streamId currentStreamsOnRemoteServer
  in
  insertAggregator streamId server state{ remoteAggregatorLocations = Map.insert server newStreams remoteAggregatorLocations }

insertAggregator :: StreamId -> Server -> State -> State
insertAggregator streamId server state@{ aggregatorLocations } =
    state { aggregatorLocations = Map.insert streamId server aggregatorLocations }

-- Called by IngestAggregator to indicate stream stopped on this node
announceStreamStopped :: StreamId -> Effect Unit
announceStreamStopped streamId =
  Gen.doCast serverName
    \state@{ aggregatorLocations, thisServer, transPoPApi:{announceStreamStopped: transPoP_announceStreamStopped} } -> do
      logInfo "Local stream stopped" {streamId}
      _ <- Bus.raise bus (IngestAggregatorExited streamId thisServer)
      _ <- sendToIntraSerfNetwork state "streamStopped" (IMStreamState StreamStopped streamId (extractAddress thisServer))
      _ <- transPoP_announceStreamStopped streamId thisServer

      let
        newAggregatorLocations = Map.delete streamId aggregatorLocations
      pure $ Gen.CastNoReply state { aggregatorLocations = newAggregatorLocations }

-- Called by TransPoP to indicate stream that is present on a node in another PoP
announceRemoteStreamIsAvailable :: StreamId -> Server -> Effect Unit
announceRemoteStreamIsAvailable streamId server =
  Gen.doCast serverName
    \state@{ aggregatorLocations } -> do
      --logIfNew streamId aggregatorLocations "New remote stream is avaiable" {streamId}
      _ <- sendToIntraSerfNetwork state "streamAvailable" (IMStreamState StreamAvailable streamId (extractAddress server))

      pure $ Gen.CastNoReply $ insertAggregator streamId server state

-- Called by TransPoP to indicate stream that has stopped on a node in another PoP
announceRemoteStreamStopped :: StreamId -> Server -> Effect Unit
announceRemoteStreamStopped streamId server =
  Gen.doCast serverName
    \state@{ aggregatorLocations } -> do
      logInfo "Remote stream has stopped" {streamId}
      -- TODO - serverAddress or server for bus message???
      _ <- Bus.raise bus (IngestAggregatorExited streamId server)
      _ <- sendToIntraSerfNetwork state "streamStopped" (IMStreamState StreamStopped streamId (extractAddress server))
      let
        newAggregatorLocations = Map.delete streamId aggregatorLocations
      pure $ Gen.CastNoReply state { aggregatorLocations = newAggregatorLocations }

-- Called by TransPoP to indicate that it is acting as this PoP's leader
announceTransPoPLeader :: Effect Unit
announceTransPoPLeader =
  Gen.doCast serverName
    \state@{ thisServer, transPoPApi:{handleRemoteLeaderAnnouncement: transPoP_announceTransPoPLeader} } ->
    do
      _ <- transPoP_announceTransPoPLeader thisServer
      _ <- sendToIntraSerfNetwork state "transPoPLeader" (IMTransPoPLeader $ extractAddress thisServer)
      pure $ Gen.CastNoReply state




--------------------------------------------------------------------------------
-- Helper for consistent behaivour around launching resources within a PoP
--------------------------------------------------------------------------------
launchLocalOrRemoteGeneric :: (ServerLoad -> Boolean) -> (ServerLoad -> Effect Unit) -> (ServerLoad -> Effect Unit) -> Effect (ResourceResp Server)
launchLocalOrRemoteGeneric pred launchLocal launchRemote = do
  idleServerResp <- getIdleServer pred
  launchResp <-  -- TODO - currently unit - maybe allow some sort of check?
    case idleServerResp of
      Right (Local local) ->
        launchLocal local
      Right (Remote remote) ->
        launchRemote remote
      Left NoCapacity ->
        pure unit
  pure $ (map serverLoadToServer) <$> idleServerResp


--------------------------------------------------------------------------------
-- Gen Server methods
--------------------------------------------------------------------------------
startLink :: {config :: Config.IntraPoPAgentConfig, transPoPApi :: Config.TransPoPAgentApi} -> Effect StartLinkResult
startLink args = Gen.startLink serverName (init args) handleInfo

init :: {config :: Config.IntraPoPAgentConfig, transPoPApi :: Config.TransPoPAgentApi} -> Effect State
init { config: config@{rejoinEveryMs
                      , expireThresholdMs
                      , expireEveryMs
                      , livenessMs
                      }
     , transPoPApi}
               = do
  logInfo "Intra-PoP Agent Starting" {config: config}
  _ <- Gen.registerExternalMapping serverName (\m -> IntraPoPSerfMsg <$> (Serf.messageMapper m))
  _ <- Timer.sendAfter serverName 0 JoinAll
  _ <- Timer.sendEvery serverName rejoinEveryMs JoinAll
  _ <- Timer.sendEvery serverName livenessMs Liveness
  _ <- Timer.sendEvery serverName expireEveryMs GarbageCollect
  rpcBindIp <- Env.privateInterfaceIp
  thisServer <- PoPDefinition.getThisServer
  let
    serfRpcAddress =
      { ip: show rpcBindIp
      , port: config.rpcPort
      }
  membersResp <- Serf.members serfRpcAddress
  streamResp <- Serf.stream serfRpcAddress
  _ <- case streamResp of
         Left error -> do
           logInfo "Could not connect to IntraPoP Serf Agent" { error: error }
           _ <- Erl.sleep (wrap 100) -- Just so we don't spin like crazy...
           unsafeCrashWith ("could_not_connect_stream")
         Right r ->
           pure r

  serversInPoP <- PoPDefinition.serversInThisPoPByAddress
  thisServerRef <- makeRef

  let
    busy = wrap 100.0 :: Load
    members = membersResp
              # hush
              # fromMaybe nil
              # filterMap (\member@{name} ->
                            let
                              serverAddress = ServerAddress name
                              serverToMemberInfo server =
                                { server: server
                                , load: busy
                                , serfMember: member
                                }
                            in
                              map (Tuple serverAddress) $ map serverToMemberInfo $ Map.lookup serverAddress serversInPoP
                          )
              # fromFoldable
  pure
    { config
    , transPoPApi
    , serfRpcAddress
    , egestLocations: MultiMap.empty
    , streamRelayLocations: EMap.empty
    , aggregatorLocations: Map.empty
    , remoteAggregatorLocations: Map.empty
    , thisServer: thisServer
    , currentTransPoPLeader: Nothing
    , members
    , load: wrap 0.0
    , thisServerRef
    , serverRefs: EMap.empty
    , expireThreshold: wrap expireThresholdMs
    , lamportClocks: { streamStateClocks: Map.empty
                     , egestStateClocks: Map.empty
                     , loadClocks: Map.empty
                     , popLeaderClocks: Map.empty
                     , livenessClocks : Map.empty
                     }
    }

handleInfo :: Msg -> State -> Effect (CastResult State)
handleInfo msg state = case msg of
  JoinAll -> do
    _ <- joinAllSerf state
    pure $ CastNoReply state

  GarbageCollect ->
    CastNoReply <$> (garbageCollect state)

  IntraPoPSerfMsg imsg ->
    CastNoReply <$> handleIntraPoPSerfMsg imsg state

  Liveness -> do
    sendToIntraSerfNetwork state "liveness" (IMLiveness (extractAddress state.thisServer) state.thisServerRef )
    pure $ CastNoReply state


handleIntraPoPSerfMsg :: (Serf.SerfMessage IntraMessage) -> State -> Effect State
handleIntraPoPSerfMsg imsg state@{ transPoPApi: {handleRemoteLeaderAnnouncement}
                                 , thisServer
                                 , lamportClocks
                                 } =
  case imsg of
    Serf.MemberAlive members -> membersAlive members state
    Serf.MemberLeaving -> pure state
    Serf.MemberLeft members -> membersLeft members state
    Serf.MemberFailed -> pure state

    Serf.StreamFailed -> do
      logInfo "Lost connection to IntraPoP Serf Agent" {}
      _ <- Erl.sleep (wrap 100) -- Just so we don't spin like crazy...
      -- TODO send a "stepping down message?" - except without Serf I guess we can't
      unsafeCrashWith ("lost_serf_connection")

    Serf.UserEvent name ltime coalesce intraMessage -> do
      let
        screenAndHandle address getClockElement setClockElement handler = do
          mTuple <- screenOriginAndMessageClock thisServer ltime address (getClockElement lamportClocks)
          case mTuple of
            Nothing -> pure state
            Just (Tuple newClockState server) ->
              handler server state{lamportClocks = setClockElement newClockState}

      case intraMessage of
        IMStreamState stateChange streamId address -> do
          let
            get = _.streamStateClocks
            set newClocks = lamportClocks{streamStateClocks = newClocks}
          screenAndHandle address get set (handleStreamStateChange stateChange streamId)

        IMEgestState stateChange streamId address -> do
          let
            get = _.egestStateClocks
            set newClocks = lamportClocks{egestStateClocks = newClocks}
          screenAndHandle address get set (handleEgestStateChange stateChange streamId)

        IMServerLoad address load -> do
          let
            get = _.loadClocks
            set newClocks = lamportClocks{loadClocks = newClocks}
          screenAndHandle address get set (handleLoadChange address load)

        IMTransPoPLeader address -> do
          let
            get = _.popLeaderClocks
            set newClocks = lamportClocks{popLeaderClocks = newClocks}
          screenAndHandle address get set handleTransPoPLeader

        IMLiveness address ref -> do
          let
            get = _.livenessClocks
            set newClocks = lamportClocks{livenessClocks = newClocks}
          screenAndHandle address get set (handleLiveness ref)


handleLiveness :: Ref -> Server -> State -> Effect State
handleLiveness ref server state = do
  newState <- case EMap.lookup server state.serverRefs of
    Nothing ->
      pure state
    Just curRef
      | ref == curRef ->
        pure state
      | otherwise ->
        garbageCollectServer state server
  newServerRefs <- EMap.insert' server ref state.serverRefs
  pure newState { serverRefs = newServerRefs }


handleTransPoPLeader :: Server -> State -> Effect State
handleTransPoPLeader server state =
   pure state{ currentTransPoPLeader = Just server }

handleLoadChange :: ServerAddress -> Load -> Server -> State -> Effect State
handleLoadChange address load server state =
  let
    newMembers = alter (map (\ memberInfo -> memberInfo { load = load })) address state.members
  in
   pure state{ members = newMembers }


handleStreamStateChange :: StreamState -> StreamId -> Server -> State -> Effect State
handleStreamStateChange streamState streamId server state =
  case streamState of
    StreamAvailable -> do
      -- streamAvailable on some other node in this PoP - we need to tell Trans-PoP
      logInfo "StreamAvailable on remote node" { streamId: streamId, remoteNode: server }

      _ <- state.transPoPApi.announceStreamIsAvailable streamId server
      --_ <- logIfNew streamId state.aggregatorLocations "StreamAvailable on remote node" { streamId: streamId, remoteNode: server }

      pure $ insertRemoteAggregator streamId server state

    StreamStopped -> do
      -- streamStopped on some other node in this PoP - we need to tell Trans-PoP
      -- TODO - we do this in a bunch of places
      _ <- Bus.raise bus $ IngestAggregatorExited streamId server
      logInfo "StreamStopped on remote node" { streamId: streamId, remoteNode: server }
      _ <- state.transPoPApi.announceStreamStopped streamId server
      let
        newAggregatorLocations = Map.delete streamId state.aggregatorLocations
      pure $ state { aggregatorLocations = newAggregatorLocations }

handleEgestStateChange :: EgestState -> StreamId -> Server -> State -> Effect State
handleEgestStateChange stateChange streamId server state =
  case stateChange of
    EgestAvailable -> do
        -- egestAvailable on some other node in this PoP
        logInfo "EgestAvailable on remote node" { streamId: streamId, remoteNode: server }
        insertEgest streamId server state

    EgestStopped -> do
        logInfo "EgestStopped on remote node" { streamId: streamId, remoteNode: server }
        let
          newEgestLocations = MultiMap.delete streamId server state.egestLocations
        pure $ state { egestLocations = newEgestLocations }


--------------------------------------------------------------------------------
-- Internal functions
--------------------------------------------------------------------------------
serverName :: ServerName State Msg
serverName = Names.intraPoPName

logIfNew :: forall a v. StreamId -> EMap StreamId v -> String -> Record a -> Effect Unit
logIfNew streamId m str metadata =
  if EMap.member streamId m
  then pure unit
  else void $ logInfo str metadata

sendToIntraSerfNetwork :: State -> String -> IntraMessage -> Effect Unit
sendToIntraSerfNetwork state name msg = do
  result <- Serf.event state.serfRpcAddress name msg false
  _ <- maybeLogError "Intra-PoP serf event failed" result {}
  pure unit


membersAlive :: (List Serf.SerfMember) -> State -> Effect State
membersAlive members state = do
  logInfo "Members Alive" { members: _.name <$> members }

  serversInPoP <- PoPDefinition.serversInThisPoPByAddress

  let
    makeMemberInfo member@{ name } server =
      { serfMember: member
      , server: server
      , load: wrap 100.0 :: Load
      }

    newMembers
      = members
        # filterMap (\ member@{ name } ->
                      let
                        address = (ServerAddress name)
                      in
                       Map.lookup address serversInPoP
                       # map (\server -> Tuple address (makeMemberInfo member server))
                    )
        # foldl (\acc (Tuple address memberInfo) -> Map.insert address memberInfo acc) state.members

  pure state { members = newMembers }

membersLeft :: (List Serf.SerfMember) -> State -> Effect State
membersLeft members state = do
  logInfo "Members Left" { members: _.name <$> members }
  let
    newMembers = foldl (\acc { name } -> Map.delete (ServerAddress name) acc) state.members members
  pure state { members = newMembers }

garbageCollect :: State -> Effect State
garbageCollect state@{ expireThreshold
                     , streamRelayLocations
                     , egestLocations
                     , remoteAggregatorLocations
                     , serverRefs
                     } =
  do
    now <- Erl.systemTimeMs
    let
      threshold = now - expireThreshold
      Tuple newServerRefs garbageRefs = EMap.garbageCollect2 threshold serverRefs
      garbageServers = fst <$> garbageRefs
      newStreamRelayLocations = EMap.garbageCollect threshold streamRelayLocations
      newEgestLocations = MultiMap.garbageCollect threshold egestLocations
      --List (Tuple server (Tuple ref Map streamId -> Unit))


    newState <- foldM  garbageCollectServer state garbageServers

    pure newState{ streamRelayLocations = newStreamRelayLocations
                 , egestLocations = newEgestLocations
                 , serverRefs = newServerRefs
                 }


garbageCollectServer :: State -> Server -> Effect State
garbageCollectServer state server = do
  -- TODO - add a server decomissioning message to do this cleanly
  logWarning "server liveness timeout" {server}
  newAggregatorLocations <- garbageCollectAggregators state state.aggregatorLocations server
  pure state { remoteAggregatorLocations = Map.delete server state.remoteAggregatorLocations
             , aggregatorLocations = newAggregatorLocations
             }

garbageCollectAggregators :: State -> AggregatorLocations -> Server -> Effect AggregatorLocations
garbageCollectAggregators state locations server  = do
  case Map.lookup server state.remoteAggregatorLocations of
    Nothing -> pure locations
    Just garbageStreams -> do
      logWarning "remoteStream liveness timeout" {garbageStreams}
      foldM (garbageCollectAggregator server) locations garbageStreams

garbageCollectAggregator :: Server -> AggregatorLocations -> StreamId -> Effect AggregatorLocations
garbageCollectAggregator server locations streamId = do
  let _ = spy "garbageCollectAggregator" {streamId, server}
  Bus.raise bus (IngestAggregatorExited streamId server)
  pure $ Map.delete streamId locations


joinAllSerf :: State -> Effect Unit
joinAllSerf { config, serfRpcAddress, members } =
  let
    serverAddressToSerfAddress (ServerAddress s) =
      { ip: s
      , port: config.bindPort
      }
  in
    do
      allOtherServers <- PoPDefinition.getOtherServersForThisPoP
      let
        toJoin = Map.keys $ Map.difference (toMap allOtherServers) members
      if length toJoin < (length allOtherServers) / 2 then
        -- We already know about more than 1/2 the network - no need to issue any joins
        pure unit
      else do
        let
          joinAsync :: ServerAddress -> Effect (Process Unit)
          joinAsync addr =
            spawnLink
              ( \_ -> do
                  result <- Serf.join serfRpcAddress (singleton $ serverAddressToSerfAddress addr) true
                  _ <- maybeLogError "Intra-PoP serf join failed" result {}
                  pure unit
              )
        traverse_ joinAsync toJoin
  where
  toMap :: forall a. List a -> Map a Unit
  toMap list = foldl (\acc item -> Map.insert item unit acc) Map.empty list



screenMessage :: Server -> LamportClock -> ServerAddress -> Map ServerAddress LamportClock -> Effect (Maybe (Tuple (Map ServerAddress LamportClock) Server))
screenMessage = screenOriginAndMessageClock


screenOriginAndMessageClock :: Server -> LamportClock -> ServerAddress -> Map ServerAddress LamportClock -> Effect (Maybe (Tuple (Map ServerAddress LamportClock) Server))
screenOriginAndMessageClock (Server thisServer) msgClock messageServerAddress lastClockByServer =
  if Map.lookup messageServerAddress lastClockByServer # maybe false (_ >= msgClock)
  then do
    pure Nothing
  else
    if thisServer.address == messageServerAddress then
      pure Nothing
    else do
      mLocation <- PoPDefinition.whereIsServer messageServerAddress
      case mLocation of
        Nothing -> do
          pure Nothing
        Just location ->
          pure $ Just $ Tuple (Map.insert messageServerAddress msgClock lastClockByServer) $ toServer messageServerAddress location
          -- pure $ Just { clockState : Map.insert messageServerAddress msgClock lastClockByServer
          --             , from : toServer messageServerAddress location
          --           }


serverLoad :: MemberInfo -> ServerLoad
serverLoad {server, load} = toServerLoad server load


--------------------------------------------------------------------------------
-- Log helpers
--------------------------------------------------------------------------------
domains :: List Atom
domains = serverName # Names.toDomain # singleton

logInfo :: forall a. Logger a
logInfo = domainLog Logger.info

logWarning :: forall a. Logger a
logWarning = domainLog Logger.warning

domainLog :: forall a. Logger {domain :: List Atom, misc :: a} -> Logger a
domainLog = Logger.doLog domains

maybeLogError :: forall a b c d e. Union b (error :: e) c => Nub c d => String -> Either e a -> Record b  -> Effect Unit
maybeLogError _ (Right _) _ = pure unit
maybeLogError msg (Left err) metadata = do
  _ <- logInfo msg (Record.merge metadata {error: err})
  pure unit
