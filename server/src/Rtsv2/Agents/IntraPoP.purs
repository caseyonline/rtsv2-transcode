module Rtsv2.Agents.IntraPoP
  ( startLink

    -- This data is essentially global - within and across PoPs
  , announceLocalAggregatorIsAvailable
  , announceLocalAggregatorStopped
  , announceOtherPoPAggregatorIsAvailable
  , announceOtherPoPAggregatorStopped


    -- Data scoped to this PoP
  , announceLocalEgestIsAvailable
  , announceLocalEgestStopped
  , announceLocalRelayIsAvailable

  , announceLoad
  , announceTransPoPLeader


    -- State queries

  , getPublicState

  , whereIsIngestAggregator
  , whereIsStreamRelay
  , whereIsEgest
  , getIdleServer
  , currentTransPoPLeader

  , isStreamIngestAvailable
  , isIngestActive


    -- Helper - not sure it's used... -- TODO
  , launchLocalOrRemoteGeneric

  , health
  , bus
  , IntraMessage(..)
  , EventType(..)
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
import Erl.Atom (Atom)
import Erl.Data.List (List, head, index, length, nil, singleton, sortBy, take, uncons)
import Erl.Data.Map (Map, alter, fromFoldable, values)
import Erl.Data.Map as Map
import Erl.Process (Process, spawnLink)
import Erl.Utils (Ref, makeRef)
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
import Rtsv2.Agents.Locator.Types (LocalOrRemote(..), NoCapacity(..), ResourceResp, ServerSelectionPredicate, fromLocalOrRemote)
import Rtsv2.Config as Config
import Rtsv2.Env as Env
import Rtsv2.Health (Health, percentageToHealth)
import Rtsv2.Names as Names
import Rtsv2.PoPDefinition as PoPDefinition
import Serf (IpAndPort, LamportClock)
import Serf as Serf
import Shared.Stream (StreamAndVariant(..), StreamId)
import Shared.Types (Load, Milliseconds, Server(..), ServerAddress(..), ServerLoad(..), extractAddress, extractPoP, serverLoadToServer, toServer, toServerLoad)
import Shared.Types.Agent.State as PublicState


type MemberInfo = { serfMember :: Serf.SerfMember
                  , load :: Load
                  , server :: Server
                  }

type LamportClocks =
  { aggregatorClocks :: Map ServerAddress LamportClock
  , egestClocks      :: Map ServerAddress LamportClock
  , relayClocks      :: Map ServerAddress LamportClock
  , loadClocks       :: Map ServerAddress LamportClock
  , popLeaderClocks  :: Map ServerAddress LamportClock
  , vmLivenessClocks   :: Map ServerAddress LamportClock
  , assetLivenessClocks   :: Map ServerAddress LamportClock
  }

type StreamLocations = { byStreamId :: Map StreamId (Set Server)
                       , byServer   :: Map Server (Set StreamId)
                       }

type State
  = { config                :: Config.IntraPoPAgentConfig
    , healthConfig          :: Config.HealthConfig
    , transPoPApi           :: Config.TransPoPAgentApi
    , serfRpcAddress        :: IpAndPort
    , currentTransPoPLeader :: Maybe Server

    , relays                :: StreamLocations
    , aggregators           :: StreamLocations
    , egests                :: StreamLocations

    , thisServerRef         :: Ref
    , serverRefs            :: EMap Server Ref

    , thisServer            :: Server
    , load                  :: Load
    , members               :: Map ServerAddress MemberInfo
    , expireThreshold       :: Milliseconds
    , lamportClocks         :: LamportClocks
    }

data EventType = Available
               | Stopped

data IntraMessage = IMAggregatorState EventType StreamId ServerAddress
                  | IMEgestState EventType StreamId ServerAddress
                  | IMRelayState EventType StreamId ServerAddress

                  | IMAggregatorLiveness ServerAddress (List StreamId)
                  | IMEgestLiveness ServerAddress (List StreamId)
                  | IMRelayLiveness ServerAddress (List StreamId)

                  | IMServerLoad ServerAddress Load
                  | IMTransPoPLeader ServerAddress

                  | IMVMLiveness ServerAddress Ref

data Msg
  = JoinAll
  | GarbageCollect
  | IntraPoPSerfMsg (Serf.SerfMessage IntraMessage)
  | VMLiveness
  | AssetLiveness

data IntraPoPBusMessage =
  IngestAggregatorExited StreamId Server



--------------------------------------------------------------------------------
-- API
--------------------------------------------------------------------------------
health :: Effect Health
health =
  Gen.doCall serverName \state@{ members, healthConfig } -> do
    allOtherServers <- PoPDefinition.getOtherServersForThisPoP
    let
      currentHealth = percentageToHealth healthConfig $ (Map.size members) * 100 / ((length allOtherServers) + 1)
    pure $ CallReply currentHealth state

bus :: Bus.Bus IntraPoPBusMessage
bus = Bus.bus "intrapop_bus"



getPublicState :: Effect PublicState.IntraPoP
getPublicState = exposeState publicState serverName
  where
    toLocations (Tuple k v) = { streamId: k
                              , servers: Set.toUnfoldable v}
    publicState state =
      { aggregatorLocations: toLocations <$>  Map.toUnfoldable state.aggregators.byStreamId
      , relayLocations: toLocations <$>  Map.toUnfoldable state.relays.byStreamId
      }

isStreamIngestAvailable :: StreamId -> Effect Boolean
isStreamIngestAvailable streamId =
  Gen.call serverName \state@{ aggregators } ->
    CallReply (Map.member streamId aggregators.byStreamId) state


--TODO....
isIngestActive :: StreamAndVariant -> Effect Boolean
isIngestActive (StreamAndVariant s v) = Gen.call serverName \state -> CallReply false state



-- TODO - we should be calling the prime' versions and handling that there might, in fact be more than
-- one instance of things we'd like to be singletons
whereIsIngestAggregator :: StreamId -> Effect (Maybe Server)
whereIsIngestAggregator streamId  = head <$> whereIs _.aggregators streamId

whereIsStreamRelay :: StreamId -> Effect (Maybe (LocalOrRemote Server))
whereIsStreamRelay streamId  = head <$> (map $ map serverLoadToServer) <$> whereIsStreamRelay' streamId

whereIsEgest :: StreamId -> Effect (List ServerLoad)
whereIsEgest streamId = (map fromLocalOrRemote) <$> whereIsEgest' streamId

whereIsStreamRelay' :: StreamId -> Effect (List (LocalOrRemote ServerLoad))
whereIsStreamRelay' = whereIsWithLoad _.relays

whereIsEgest' :: StreamId -> Effect (List (LocalOrRemote ServerLoad))
whereIsEgest' = whereIsWithLoad _.egests


whereIs ::(State -> StreamLocations) -> StreamId -> Effect (List Server)
whereIs extractMap streamId =
  Gen.call serverName
  \state@{thisServer, members} ->
    let
      streamLocations = _.byStreamId $ extractMap state
      mLocations = Map.lookup streamId streamLocations :: Maybe (Set Server)
      locations = maybe nil Set.toUnfoldable mLocations
    in
    CallReply locations state


whereIsWithLoad ::(State -> StreamLocations) -> StreamId -> Effect (List (LocalOrRemote ServerLoad))
whereIsWithLoad extractMap streamId =
  Gen.call serverName
  \state@{thisServer, members} ->
    let
      streamLocations = _.byStreamId $ extractMap state
      withLoad (Server el) = serverLoad <$> Map.lookup el.address members
      mLocations = Map.lookup streamId streamLocations :: Maybe (Set Server)

      locations = maybe nil Set.toUnfoldable mLocations
      filtered = filterMap withLoad locations
    in
    CallReply (toLocalOrRemote thisServer <$> filtered) state
  where
    toLocalOrRemote thisSever server =
      if extractAddress thisSever == extractAddress server
      then Local server
      else Remote server


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


type EventHandler = State -> StreamId -> Server -> Effect Unit
type GetClocks = LamportClocks -> Map ServerAddress LamportClock
type SetClocks = Map ServerAddress LamportClock -> LamportClocks -> LamportClocks

type ClockLens = { get :: LamportClocks -> Map ServerAddress LamportClock
                  , set :: Map ServerAddress LamportClock -> LamportClocks -> LamportClocks
                  }

type LocationLens = { get :: State -> StreamLocations
                     , set :: StreamLocations -> State -> State
                     }

-- TODO should the handlers only be called on state transitions
-- TODO rename SlotAssetHandler (StreamIdAssetHandler)?
type AssetHandler =
  { name              :: String

  , availableLocal    :: EventHandler
  , availableThisPoP  :: EventHandler
  , availableOtherPoP :: EventHandler
  , stoppedLocal      :: EventHandler
  , stoppedThisPoP    :: EventHandler
  , stoppedOtherPoP   :: EventHandler

  , clockLens         :: ClockLens
  , locationLens      :: LocationLens
    -- TODO -- Maybe add isSingleton - to generate warnings if the sets have more than one entry?
  }


aggregatorHandler :: AssetHandler
aggregatorHandler
  = { name              : "aggregator"
    , availableLocal    : availableLocal
    , availableThisPoP  : availableThisPoP
    , availableOtherPoP : availableOtherPoP
    , stoppedLocal      : stoppedLocal
    , stoppedThisPoP    : stoppedThisPoP
    , stoppedOtherPoP   : stoppedOtherPoP

    , clockLens         : clockLens
    , locationLens      : locationLens
    }
  where
    availableLocal state streamId server = do
      logInfo "Local aggregator available" {streamId}
      sendToIntraSerfNetwork state "aggregatorAvailable" (IMAggregatorState Available streamId (extractAddress server))
      state.transPoPApi.announceAggregatorIsAvailable streamId server

    availableThisPoP state streamId server = do
      logInfo "New aggregator is available in this PoP" {streamId, server}
      state.transPoPApi.announceAggregatorIsAvailable streamId server

    availableOtherPoP state streamId server = do
      logInfo "New aggregator is available in another PoP" {streamId, server}
      sendToIntraSerfNetwork state "aggregatorAvailable" (IMAggregatorState Available streamId (extractAddress server))

    stoppedLocal state streamId server = do
      logInfo "Local aggregator stopped" {streamId}
      Bus.raise bus (IngestAggregatorExited streamId server)
      sendToIntraSerfNetwork state "aggregatorStopped" $ IMAggregatorState Stopped streamId $ extractAddress server
      state.transPoPApi.announceAggregatorStopped streamId server

    stoppedThisPoP state streamId server = do
      logInfo "Remote aggregator stopped in this PoP" {streamId, server}
      Bus.raise bus (IngestAggregatorExited streamId server)
      state.transPoPApi.announceAggregatorStopped streamId server

    stoppedOtherPoP state streamId server = do
      logInfo "Remote aggregator stopped in another PoP" {streamId, server}
      Bus.raise bus (IngestAggregatorExited streamId server)
      sendToIntraSerfNetwork state "aggregatorStopped" (IMAggregatorState Stopped streamId (extractAddress server))

    clockLens = { get : _.aggregatorClocks
                 , set : \newClocks lamportClocks -> lamportClocks{aggregatorClocks = newClocks}
                 }

    locationLens = { get : _.aggregators
                    , set : \newLocations state -> state {aggregators = newLocations}
                    }

-- Called by IngestAggregator to indicate stream on this node
announceLocalAggregatorIsAvailable :: StreamId -> Effect Unit
announceLocalAggregatorIsAvailable = announceAvailableLocal aggregatorHandler

-- Called by IngestAggregator to indicate stream stopped on this node
announceLocalAggregatorStopped :: StreamId -> Effect Unit
announceLocalAggregatorStopped = announceStoppedLocal aggregatorHandler

-- Called by TransPoP to indicate stream that is present on a node in another PoP
announceOtherPoPAggregatorIsAvailable :: StreamId -> Server -> Effect Unit
announceOtherPoPAggregatorIsAvailable = announceAvailableOtherPoP aggregatorHandler

-- Called by TransPoP to indicate stream that has stopped on a node in another PoP
announceOtherPoPAggregatorStopped :: StreamId -> Server -> Effect Unit
announceOtherPoPAggregatorStopped = announceStoppedOtherPoP aggregatorHandler


egestHandler :: AssetHandler
egestHandler
  = { name              : "egest"
    , availableLocal    : availableLocal
    , availableThisPoP  : availableThisPoP
    , availableOtherPoP : availableOtherPoP
    , stoppedLocal      : stoppedLocal
    , stoppedThisPoP    : stoppedThisPoP
    , stoppedOtherPoP   : stoppedOtherPoP

    , clockLens         : clockLens
    , locationLens      : locationLens
    }
  where
    availableLocal state streamId server = do
      logInfo "Local egest available" {streamId}
      sendToIntraSerfNetwork state "egestAvailable" (IMEgestState Available streamId $ extractAddress server)

    availableThisPoP state streamId server = do
      logInfo "New egest is avaiable in this PoP" {streamId, server}

    availableOtherPoP state streamId server = do
      -- Not expecting any of these
      logWarning "New egest is avaiable in another PoP" {streamId, server}

    stoppedLocal state streamId server = do
      logInfo "Local egest stopped" {streamId}
      sendToIntraSerfNetwork state "egestStopped" $ IMEgestState Stopped streamId $ extractAddress server

    stoppedThisPoP state streamId server = do
      logInfo "Remote egest stopped" {streamId, server}

    stoppedOtherPoP state streamId server = do
      -- Not expecting any of these
      logWarning "Egest stopped in another PoP" {streamId, server}

    clockLens = { get : _.egestClocks
                 , set : \newClocks lamportClocks -> lamportClocks{egestClocks = newClocks}
                 }

    locationLens = { get : _.egests
                    , set : \newLocations state -> state {egests = newLocations}
                    }

-- Called by EgestAgent to indicate egest on this node
announceLocalEgestIsAvailable :: StreamId -> Effect Unit
announceLocalEgestIsAvailable = announceAvailableLocal egestHandler

announceLocalEgestStopped :: StreamId -> Effect Unit
announceLocalEgestStopped = announceStoppedLocal egestHandler


--TODO - relay and egest are literally identical other than the names...
relayHandler :: AssetHandler
relayHandler
  = { name              : "relay"
    , availableLocal    : availableLocal
    , availableThisPoP  : availableThisPoP
    , availableOtherPoP : availableOtherPoP
    , stoppedLocal      : stoppedLocal
    , stoppedThisPoP    : stoppedThisPoP
    , stoppedOtherPoP   : stoppedOtherPoP

    , clockLens         : clockLens
    , locationLens      : locationLens
    }
  where
    availableLocal state streamId server = do
      logInfo "Local relay available" {streamId}
      sendToIntraSerfNetwork state "relayAvailable" (IMRelayState Available streamId $ extractAddress server)

    availableThisPoP state streamId server = do
      logInfo "New relay is available in this PoP" {streamId, server}

    availableOtherPoP state streamId server = do
      -- Not expecting any of these
      logWarning "New relay is available in another PoP" {streamId, server}

    stoppedLocal state streamId server = do
      logInfo "Local relay stopped" {streamId}
      sendToIntraSerfNetwork state "relayStopped" $ IMRelayState Stopped streamId $ extractAddress server

    stoppedThisPoP state streamId server = do
      logInfo "Remote relay stopped" {streamId, server}

    stoppedOtherPoP state streamId server = do
      -- Not expecting any of these
      logWarning "Relay stopped in another PoP" {streamId, server}

    clockLens = { get : _.relayClocks
                 , set : \newClocks lamportClocks -> lamportClocks{relayClocks = newClocks}
                 }

    locationLens = { get : _.relays
                    , set : \newLocations state -> state {relays = newLocations}
                    }

-- Called by RelayAgent to indicate relay on this node
announceLocalRelayIsAvailable :: StreamId -> Effect Unit
announceLocalRelayIsAvailable = announceAvailableLocal relayHandler

announceLocalRelayStopped :: StreamId -> Effect Unit
announceLocalRelayStopped = announceStoppedLocal relayHandler


-- Builds public API for events on this server
announceAvailableLocal :: AssetHandler -> StreamId -> Effect Unit
announceAvailableLocal handler@{locationLens} streamId =
  Gen.doCast serverName
    \state@{ thisServer } -> do
      let _ = spy "announceAvailableLocal" {name: handler.name, streamId}
      handler.availableLocal state streamId thisServer
      let
        newLocations = recordLocation streamId thisServer (locationLens.get state)
      pure $ Gen.CastNoReply $ locationLens.set newLocations state

announceStoppedLocal :: AssetHandler -> StreamId -> Effect Unit
announceStoppedLocal handler@{locationLens} streamId = do
  Gen.doCast serverName
    \state@{ thisServer } -> do
      let _ = spy "stoppedLocal" {name: handler.name, streamId}
      handler.stoppedLocal state streamId thisServer
      let
        newLocations = removeLocation streamId thisServer (locationLens.get state)
      pure $ Gen.CastNoReply $ locationLens.set newLocations state


-- Builds public API for message from other PoP
announceAvailableOtherPoP :: AssetHandler -> StreamId -> Server -> Effect Unit
announceAvailableOtherPoP handler@{locationLens} streamId server =
  Gen.doCast serverName
    \state -> do
      let _ = spy "announceAvailableOtherPoP" {name: handler.name, streamId}
      handler.availableOtherPoP state streamId server
      let
        newLocations = recordLocation streamId server (locationLens.get state)
      let _ = spy "newLocations" {newLocations}
      pure $ Gen.CastNoReply $ locationLens.set newLocations state

announceStoppedOtherPoP :: AssetHandler -> StreamId -> Server -> Effect Unit
announceStoppedOtherPoP handler@{locationLens} streamId server =
  Gen.doCast serverName
    \state -> do
      let _ = spy "stoppedOtherPoP" {name: handler.name, streamId}
      handler.stoppedOtherPoP state streamId server
      let
        newLocations = removeLocation streamId server (locationLens.get state)
      let _ = spy "newLocations" {newLocations}
      pure $ Gen.CastNoReply $ locationLens.set newLocations state

-- Private API called in response to IntraPoP bus message relating to events in this PoP
-- We need to not re-broadcast messages from other PoPs that we have already relayed
maybeAnnounceAvailableThisPoP :: AssetHandler -> StreamId -> Server -> State -> Effect State
maybeAnnounceAvailableThisPoP handler@{locationLens} streamId server state = do
  if extractPoP server == extractPoP state.thisServer
  then do
    handler.availableThisPoP state streamId server
    let
      newLocations = recordLocation streamId server (locationLens.get state)
    pure $ locationLens.set newLocations state
  else
    pure state

maybeStopThisPoP :: AssetHandler -> StreamId -> Server -> State -> Effect State
maybeStopThisPoP handler@{locationLens} streamId server state = do
  if extractPoP server == extractPoP state.thisServer
  then do
    handler.stoppedThisPoP state streamId server
    let
      newLocations = removeLocation streamId server (locationLens.get state)
    pure $ locationLens.set newLocations state
  else
    pure state


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
                      , vmLivenessMs
                      , assetLivenessMs
                      }
     , transPoPApi}
               = do
  logInfo "Intra-PoP Agent Starting" {config: config}
  healthConfig <- Config.healthConfig
  Gen.registerExternalMapping serverName (\m -> IntraPoPSerfMsg <$> (Serf.messageMapper m))
  void $ Timer.sendAfter serverName 0 JoinAll
  void $ Timer.sendEvery serverName rejoinEveryMs JoinAll
  void $ Timer.sendEvery serverName vmLivenessMs VMLiveness
  void $ Timer.sendEvery serverName assetLivenessMs AssetLiveness
  void $ Timer.sendEvery serverName expireEveryMs GarbageCollect
  rpcBindIp <- Env.privateInterfaceIp
  thisServer <- PoPDefinition.getThisServer
  let
    _ = spy "healthConfig" healthConfig
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
    emptyLocations = { byStreamId : Map.empty
                     , byServer   : Map.empty
                     }
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
    , healthConfig
    , transPoPApi
    , serfRpcAddress
    , currentTransPoPLeader: Nothing

    , relays: emptyLocations
    , aggregators:  emptyLocations
    , egests :  emptyLocations

    , thisServerRef
    , serverRefs: EMap.empty

    , thisServer: thisServer
    , load: wrap 0.0
    , members
    , expireThreshold: wrap expireThresholdMs
    , lamportClocks: { aggregatorClocks: Map.empty
                     , egestClocks: Map.empty
                     , relayClocks: Map.empty
                     , loadClocks: Map.empty
                     , popLeaderClocks: Map.empty
                     , vmLivenessClocks : Map.empty
                     , assetLivenessClocks : Map.empty
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

  VMLiveness -> do
    sendToIntraSerfNetwork state "vmLiveness" (IMVMLiveness (extractAddress state.thisServer) state.thisServerRef )
    pure $ CastNoReply state

  AssetLiveness -> do
    let
--      egestKeys = maybe nil Set.toUnfoldable $ Map.lookup state.thisServer  state.egests.byServer
      aggregatorKeys = maybe nil Set.toUnfoldable $ Map.lookup state.thisServer state.aggregators.byServer
      egestKeys = maybe nil Set.toUnfoldable $ Map.lookup state.thisServer state.egests.byServer
      relayKeys = maybe nil Set.toUnfoldable $ Map.lookup state.thisServer state.relays.byServer

    -- TODO - make sure the message fits into a single packet
    -- TODO - randomise and maybe separate the time interval between the messages?
    sendToIntraSerfNetwork state "assetLiveness" (IMAggregatorLiveness (extractAddress state.thisServer) aggregatorKeys)
    sendToIntraSerfNetwork state "assetLiveness" (IMEgestLiveness (extractAddress state.thisServer) egestKeys)
    sendToIntraSerfNetwork state "assetLiveness" (IMRelayLiveness (extractAddress state.thisServer) relayKeys)
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
        screenAndHandle :: ServerAddress -> ClockLens -> (Server -> State -> Effect State) -> Effect State
        screenAndHandle address clockLens messageHandler = do
          mTuple <- screenOriginAndMessageClock thisServer ltime address (clockLens.get lamportClocks)
          case mTuple of
            Nothing -> pure state
            Just (Tuple newClockState server) ->
              messageHandler server state{lamportClocks = clockLens.set newClockState lamportClocks}

      case intraMessage of
        IMAggregatorState eventType streamId address -> do
          screenAndHandle address aggregatorHandler.clockLens $ handleThisPoPAsset eventType aggregatorHandler streamId
        IMAggregatorLiveness address aggregatorStreamIds -> do
          screenAndHandle address aggregatorHandler.clockLens (handleAssetLiveness aggregatorHandler aggregatorStreamIds)

        IMEgestState eventType streamId address -> do
          screenAndHandle address egestHandler.clockLens $ handleThisPoPAsset eventType egestHandler streamId
        IMEgestLiveness address egestStreamIds -> do
          screenAndHandle address egestHandler.clockLens (handleAssetLiveness egestHandler egestStreamIds)

        IMRelayState eventType streamId address -> do
          screenAndHandle address relayHandler.clockLens $ handleThisPoPAsset eventType relayHandler streamId
        IMRelayLiveness address relayStreamIds -> do
          screenAndHandle address relayHandler.clockLens (handleAssetLiveness relayHandler relayStreamIds)

        IMServerLoad address load -> do
          let
            clockLens = { get : _.loadClocks
                         , set : \newClocks lc -> lc{loadClocks = newClocks}
                         }
          screenAndHandle address clockLens (handleLoadChange address load)

        IMTransPoPLeader address -> do
          let
            clockLens = { get : _.popLeaderClocks
                         , set : \newClocks lc -> lc{popLeaderClocks = newClocks}
                         }
          screenAndHandle address clockLens handleTransPoPLeader

        IMVMLiveness address ref -> do
          let
            clockLens = { get : _.vmLivenessClocks
                         , set : \newClocks lc -> lc{vmLivenessClocks = newClocks}
                         }
          screenAndHandle address clockLens (handleLiveness ref)


  where
    handleThisPoPAsset :: EventType -> AssetHandler -> StreamId -> Server -> State -> Effect State
    handleThisPoPAsset Available = maybeAnnounceAvailableThisPoP
    handleThisPoPAsset Stopped = maybeStopThisPoP


handleAssetLiveness :: AssetHandler -> List StreamId -> Server -> State -> Effect State
handleAssetLiveness handler@{locationLens} streamIds server state = do
  let
    assetLocations = locationLens.get state
    ourVersion = fromMaybe Set.empty $ Map.lookup server assetLocations.byServer
    livenessVersion = Set.fromFoldable streamIds
    newAssets = Set.difference livenessVersion ourVersion
    expiredAssets = Set.difference ourVersion livenessVersion

    processNew :: StreamLocations -> StreamId -> Effect StreamLocations
    processNew location streamId = do
      let _ = spy "processNew" {streamId, server, name: handler.name}
      handler.availableThisPoP state streamId server
      pure $ recordLocation streamId server location

    processExpired :: StreamLocations -> StreamId -> Effect StreamLocations
    processExpired location streamId = do
      let _ = spy "processExpired" {streamId, server, name: handler.name}
      handler.stoppedThisPoP state streamId server
      pure $ removeLocation streamId server location

  withNew <- foldM processNew assetLocations newAssets
  withoutExpired <- foldM processExpired withNew expiredAssets
  pure $ locationLens.set withoutExpired state


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
                     , serverRefs
                     } =
  do
    now <- Erl.systemTimeMs
    let
      threshold = now - expireThreshold
      Tuple newServerRefs garbageRefs = EMap.garbageCollect2 threshold serverRefs
      garbageServers = fst <$> garbageRefs
    foldM garbageCollectServer state{serverRefs = newServerRefs} garbageServers


garbageCollectServer :: State -> Server -> Effect State
garbageCollectServer state deadServer = do
  -- TODO - add a server decomissioning message to do this cleanly
  logWarning "server liveness timeout" {server: deadServer}
  newState <- gc "aggregators" aggregatorHandler deadServer
              =<< gc "egests" egestHandler deadServer
              =<< gc "relays" relayHandler deadServer state
  pure newState

gc :: String -> AssetHandler -> Server -> State -> Effect State
gc assetType assetHandler@{locationLens} deadServer state = do
  let
    locations = locationLens.get state
  case Map.lookup deadServer locations.byServer of
    Nothing -> pure state
    Just garbageStreams -> do
      logWarning (assetType <> " liveness timeout") {garbageStreams}
      newByStreamId <- foldM cleanUp locations.byStreamId  garbageStreams
      let
        newLocations = { byStreamId : newByStreamId
                       , byServer : Map.delete deadServer locations.byServer
                       }
      pure $ locationLens.set newLocations state
  where
    cleanUp acc streamId = do
      assetHandler.stoppedThisPoP state streamId deadServer
      pure $ mapSetDelete streamId deadServer acc



recordLocation :: StreamId -> Server -> StreamLocations -> StreamLocations
recordLocation streamId server locations =
  { byStreamId : mapSetInsert streamId server locations.byStreamId
  , byServer : mapSetInsert server streamId locations.byServer
  }

removeLocation :: StreamId -> Server -> StreamLocations -> StreamLocations
removeLocation streamId server locations =
  { byStreamId : mapSetDelete streamId server locations.byStreamId
  , byServer : mapSetDelete server streamId locations.byServer
  }

mapSetInsert :: forall k v. Ord k => Ord v => k -> v -> Map k (Set v) -> Map k (Set v)
mapSetInsert k v mapSet =
  let
    current = fromMaybe Set.empty $ Map.lookup k mapSet
  in
    Map.insert k (Set.insert v current) mapSet

mapSetDelete :: forall k v. Ord k => Ord v => k -> v -> Map k (Set v) -> Map k (Set v)
mapSetDelete k v mapSet =
  case Map.lookup k mapSet of
    Nothing -> mapSet
    Just existing ->
      let
        newSet = Set.delete v existing
      in
       if Set.isEmpty newSet
       then Map.delete k mapSet
       else Map.insert k newSet mapSet


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
                  result <- Serf.join serfRpcAddress (singleton $ serverAddressToSerfAddress addr) config.replayMessagesOnJoin

                  _ <- maybeLogError "Intra-PoP serf join failed" result {}
                  pure unit
              )
        traverse_ joinAsync toJoin
  where
  toMap :: forall a. List a -> Map a Unit
  toMap list = foldl (\acc item -> Map.insert item unit acc) Map.empty list


screenOriginAndMessageClock :: Server -> LamportClock -> ServerAddress -> Map ServerAddress LamportClock -> Effect (Maybe (Tuple (Map ServerAddress LamportClock) Server))
screenOriginAndMessageClock (Server thisServer) msgClock messageServerAddress lastClockByServer =
  if Map.lookup messageServerAddress lastClockByServer # maybe false (_ >= msgClock)
  then do
    pure Nothing
  else
    if thisServer.address == messageServerAddress then
      pure Nothing
    else do
      -- TODO - maybe cache the db for a while to prevent hot calls, or use ETS etc
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
