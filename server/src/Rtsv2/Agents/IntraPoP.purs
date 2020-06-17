-- TODO: I (Stears) have commented out various logging in this module because it's very verbose due to
--       announcements being periodically resent. We should only log if the announcements disagree with
--       our current view of the world
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
  , announceLocalRelayStopped

  , announceLoad
  , announceAcceptingRequests
  , announceTransPoPLeader
  , announceEgestSlotLookup

    -- State queries

  , getPublicState

  , currentLocalRef
  , currentRemoteRef

  , whereIsIngestAggregator
  , whereIsIngestAggregatorWithPayload
  , whereIsStreamRelay
  , whereIsStreamRelayWithLocalOrRemote
  , whereIsEgest
  , whereIsEgestWithLoad

  , getIdleServer
  , getThisIdleServer
  , selectCandidate
  , getCurrentTransPoPLeader

    -- Test helper
  , testHelper
  , health
  , bus
  , IntraPoPBusMessage(..)

  , AgentClock
  , TestHelperPayload
  ) where

import Prelude

import Bus as Bus
import Data.Either (Either(..), hush)
import Data.Filterable (filterMap)
import Data.Foldable (foldM, foldl)
import Data.FoldableWithIndex (foldlWithIndex)
import Data.Long as Long
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Set (Set)
import Data.Set as Set
import Data.Traversable (traverse, traverse_)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Ephemeral.Map (EMap)
import Ephemeral.Map as EMap
import Erl.Atom (Atom)
import Erl.Data.Binary (Binary)
import Erl.Data.List (List, head, index, length, nil, singleton, sortBy, uncons, (:))
import Erl.Data.Map (Map, alter, fromFoldable, values)
import Erl.Data.Map as Map
import Erl.Data.Tuple (Tuple2, tuple2)
import Erl.Process (Process, spawnLink)
import Erl.Utils (Ref, makeRef)
import Erl.Utils as Erl
import Heterogeneous.Folding (hfoldl)
import Logger as Logger
import Partial.Unsafe (unsafeCrashWith)
import Pinto (ServerName, StartLinkResult)
import Pinto.Gen (CallResult(..), CastResult(..))
import Pinto.Gen as Gen
import Pinto.Timer as Timer
import PintoHelper (exposeState)
import Prim.Row (class Nub, class Union)
import Record as Record
import Rtsv2.Config as Config
import Rtsv2.Env as Env
import Rtsv2.Health (percentageToHealth)
import Rtsv2.LoadTypes (LoadCheckResult, ServerSelectionPredicate)
import Rtsv2.LoadTypes as LoadTypes
import Rtsv2.Names as Names
import Rtsv2.PoPDefinition as PoPDefinition
import Rtsv2.Types (LocalOrRemote(..), ResourceFailed(..), ResourceResp)
import Serf (class SerfWireMessage, IpAndPort, LamportClock)
import Serf as Serf
import Shared.Common (Milliseconds(..))
import Shared.Rtsv2.Agent (SlotCharacteristics, emptySlotCharacteristics)
import Shared.Rtsv2.Agent.State as PublicState
import Shared.Rtsv2.JsonLd (transPoPLeaderLocationNode)
import Shared.Rtsv2.JsonLd as JsonLd
import Shared.Rtsv2.LlnwApiTypes (SlotLookupResult)
import Shared.Rtsv2.Stream (AgentKey(..), AggregatorKey, EgestKey, RelayKey(..), RtmpShortName, SlotName, agentKeyToAggregatorKey, agentKeyToEgestKey, agentKeyToRelayKey, aggregatorKeyToAgentKey, egestKeyToAgentKey)
import Shared.Rtsv2.Types (AcceptingRequests, CanaryState(..), CurrentLoad, Health, Server(..), ServerAddress(..), ServerLoad, extractAddress, extractPoP, maxLoad, minLoad, toServerLoad)
import Shared.Utils (distinctRandomNumbers)


foreign import to_wire_message :: IntraMessage -> Tuple2 String Binary
foreign import from_wire_message :: String -> Binary -> Maybe IntraMessage

foreign import to_binary :: IntraMessage -> Binary
foreign import from_binary :: Binary -> IntraMessage



type TestHelperPayload =
  { dropAgentMessages :: Boolean
  }

type MemberInfo =
  { serfMember :: Serf.SerfMember
  , load :: CurrentLoad
  , acceptingRequests :: AcceptingRequests
  , server :: Server
  }

type ServerClock = Map ServerAddress LamportClock
type AgentClock = Map (Tuple ServerAddress AgentKey) LamportClock

type AgentClocks =
  { aggregatorClocks :: AgentClock
  , egestClocks      :: AgentClock
  , relayClocks      :: AgentClock
  }

type ServerClocks =
  { loadClocks       :: ServerClock
  , popLeaderClocks  :: ServerClock
  , vmLivenessClocks :: ServerClock
  , slotLookupClocks :: ServerClock
  }

type AgentLocations = { relays                :: Locations Unit
                      , aggregators           :: Locations SlotCharacteristics
                      , egests                :: Locations Unit
                      }

type AgentPayloadAndServers payload = { payload :: payload
                                      , servers :: Set Server
                                      }

type Locations payload = { byAgentKey     :: Map AgentKey (AgentPayloadAndServers payload)
                         , byRemoteServer :: Map Server (Set AgentKey)
                         , remoteTimeouts :: Map (Tuple AgentKey Server) Milliseconds
                         }

type State
  = { config                :: Config.IntraPoPAgentConfig
    , canary                :: CanaryState
    , healthConfig          :: Config.HealthConfig
    , transPoPApi           :: Config.TransPoPAgentApi
    , llnwApiRecordSlotLookupApi :: RtmpShortName -> SlotName -> SlotLookupResult -> Effect Unit
    , serfRpcAddress        :: IpAndPort
    , currentTransPoPLeader :: Maybe Server

    , testDropAgentMessages :: Boolean

    , thisServerRef         :: Ref
    , serverRefs            :: EMap Server Ref

    , thisServer            :: Server
    , load                  :: CurrentLoad
    , acceptingRequests     :: AcceptingRequests
    , members               :: Map ServerAddress MemberInfo
    , agentClocks           :: AgentClocks
    , agentLocations        :: AgentLocations
    , serverClocks          :: ServerClocks
    }

data EventType
  = Available
  | Stopped

data IntraMessage
  = IMAggregatorState EventType AgentKey ServerAddress SlotCharacteristics
  | IMEgestState EventType AgentKey ServerAddress
  | IMRelayState EventType AgentKey ServerAddress

  | IMServerLoad ServerAddress CurrentLoad AcceptingRequests
  | IMTransPoPLeader ServerAddress
  | IMVMLiveness ServerAddress Ref
  | IMSlotLookup ServerAddress RtmpShortName SlotName SlotLookupResult

instance serfWireMessageIM :: SerfWireMessage IntraMessage where
  toWireMessage payload@(IMAggregatorState _ _ _ _) =
    to_wire_message payload

  toWireMessage payload@(IMEgestState _ _ _) =
    to_wire_message payload

  toWireMessage payload@(IMRelayState Available agentKey serverAddress) =
    tuple2 "relayAvailable" $ to_binary payload
  toWireMessage payload@(IMRelayState Stopped agentKey serverAddress) =
    tuple2 "relayStopped" $ to_binary payload

  toWireMessage payload@(IMServerLoad serverAddress currentLoad acceptingRequests) =
    tuple2 "loadUpdate" $ to_binary payload

  toWireMessage payload@(IMSlotLookup serverAddress rtmpShortName slotName slotLookupResult) =
    tuple2 "slotLookup" $ to_binary payload
  toWireMessage payload@(IMTransPoPLeader serverAddress) =
    tuple2 "transPoPLeader" $ to_binary payload
  toWireMessage payload@(IMVMLiveness serverAddress ref) =
    tuple2 "vmLiveness" $ to_binary payload


  fromWireMessage "a" payload =
    from_wire_message "a" payload
  fromWireMessage "b" payload =
    from_wire_message "b" payload
  fromWireMessage name payload =
    Just $ from_binary payload


data Msg serfPayload
  = JoinAll
  | GetAcceptingRequests (Effect AcceptingRequests)
  | GarbageCollectVM
  | GarbageCollectAgents
  | IntraPoPSerfMsg (Serf.SerfMessage IntraMessage)
  | VMLiveness
  | ReAnnounce AgentKey (AgentHandler serfPayload) (State -> Effect Unit)

data IntraPoPBusMessage
  = IngestAggregatorStarted AggregatorKey Server
  | IngestAggregatorExited AggregatorKey Server
  | StreamRelayStarted RelayKey Server
  | StreamRelayExited RelayKey Server
  | EgestStarted EgestKey Server
  | EgestExited EgestKey Server
  | VmReset Server Ref (Maybe Ref)


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

bus :: Bus.Bus String IntraPoPBusMessage
bus = Bus.bus "intrapop_bus"


testHelper :: TestHelperPayload -> Effect Unit
testHelper payload =
  Gen.doCall serverName \state -> do
    pure $ CallReply unit state {testDropAgentMessages = payload.dropAgentMessages}

getPublicState :: Effect (PublicState.IntraPoP List)
getPublicState = Gen.doCall serverName publicState
  where
    publicState state@{agentLocations, currentTransPoPLeader, thisServer} = do
      ps <- gatherState agentLocations currentTransPoPLeader
      json <- JsonLd.intraPoPStateNode ps thisServer
      pure $ CallReply json state

    gatherState agentLocations currentTransPoPLeader = do
      a <- traverse toAggregatorLocation $ Map.toUnfoldable agentLocations.aggregators.byAgentKey
      r <- traverse toRelayLocation $ Map.toUnfoldable agentLocations.relays.byAgentKey
      e <- traverse toEgestLocation $ Map.toUnfoldable agentLocations.egests.byAgentKey
      c <- traverse transPoPLeaderLocationNode currentTransPoPLeader
      pure $ { aggregatorLocations: a
             , relayLocations: r
             , egestLocations: e
             , currentTransPoPLeader: c
      }

    toAggregatorLocation (Tuple (AgentKey slotId role) {servers}) = do
      servers' <- traverse (JsonLd.aggregatorLocationNode slotId role) $ Set.toUnfoldable servers
      pure $ { slotId
             , role
             , servers: servers'
             }

    toRelayLocation (Tuple (AgentKey slotId role) {servers}) = do
      servers' <- traverse (JsonLd.relayLocationNode slotId role) $ Set.toUnfoldable servers
      pure $ { slotId
             , role
             , servers: servers'
             }

    toEgestLocation (Tuple (AgentKey slotId role) {servers}) = do
      servers' <- traverse (JsonLd.egestLocationNode slotId role) $ Set.toUnfoldable servers
      pure $ { slotId
             , role
             , servers: servers'
             }

currentLocalRef :: Effect Ref
currentLocalRef = exposeState _.thisServerRef serverName

currentRemoteRef :: Server -> Effect (Maybe Ref)
currentRemoteRef remoteServer = exposeState ((EMap.lookup remoteServer) <<<  _.serverRefs) serverName

-- TODO - we should be calling the prime' versions and handling that there might, in fact be more than
-- one instance of things we'd like to be singletons
whereIsIngestAggregator :: AggregatorKey -> Effect (Maybe Server)
whereIsIngestAggregator aggregatorKey = do
  mRes <- whereIs (_.aggregators) (aggregatorKeyToAgentKey aggregatorKey)
  pure $ head mRes

whereIsIngestAggregatorWithPayload :: AggregatorKey -> Effect (Maybe { payload :: SlotCharacteristics, server :: Server} )
whereIsIngestAggregatorWithPayload aggregatorKey = do
  mRes <- whereIsWithPayload (_.aggregators) (aggregatorKeyToAgentKey aggregatorKey)
  pure $ (\{payload, servers} ->
           case head servers of
             Nothing -> Nothing
             Just server -> Just {payload, server}
         ) =<< mRes

whereIsStreamRelay :: RelayKey -> Effect (Maybe Server)
whereIsStreamRelay (RelayKey slotId slotRole) = head <$> whereIs _.relays (AgentKey slotId slotRole)

whereIsStreamRelayWithLocalOrRemote :: RelayKey -> Effect (Maybe (LocalOrRemote Server))
whereIsStreamRelayWithLocalOrRemote (RelayKey slotId slotRole) = head <$> whereIsWithLocalOrRemote _.relays (AgentKey slotId slotRole)

whereIsEgest :: EgestKey -> Effect (List Server)
whereIsEgest egestKey = whereIs _.egests $ egestKeyToAgentKey egestKey

whereIsEgestWithLoad :: EgestKey -> Effect (List ServerLoad)
whereIsEgestWithLoad egestKey = whereIsWithLoad _.egests $ egestKeyToAgentKey egestKey

whereIs :: forall payload. (AgentLocations -> Locations payload) -> AgentKey -> Effect (List Server)
whereIs extractMap agentKey =
  Gen.call serverName
  \state@{thisServer, members, agentLocations} ->
    let
      streamLocations = _.byAgentKey $ extractMap agentLocations
      mPayloadAndServers = Map.lookup agentKey streamLocations :: Maybe (AgentPayloadAndServers payload)
      serverList = maybe nil (\{servers} -> Set.toUnfoldable servers) mPayloadAndServers
    in
    CallReply serverList state

whereIsWithLocalOrRemote :: forall payload. (AgentLocations -> Locations payload) -> AgentKey -> Effect (List (LocalOrRemote Server))
whereIsWithLocalOrRemote extractMap agentKey =
  Gen.call serverName
  \state@{thisServer, members, agentLocations} ->
    let
      streamLocations = _.byAgentKey $ extractMap agentLocations
      mPayloadAndServers = Map.lookup agentKey streamLocations :: Maybe (AgentPayloadAndServers payload)
      serverList = maybe nil (\{servers} -> Set.toUnfoldable servers) mPayloadAndServers
    in
    CallReply ((toLocalOrRemote thisServer) <$> serverList) state

whereIsWithPayload :: forall payload. (AgentLocations -> Locations payload) -> AgentKey -> Effect (Maybe { payload :: payload, servers :: (List Server)})
whereIsWithPayload extractMap agentKey =
  Gen.call serverName
  \state@{thisServer, members, agentLocations} ->
    let
      streamLocations = _.byAgentKey $ extractMap agentLocations
      mPayloadAndServers = Map.lookup agentKey streamLocations :: Maybe (AgentPayloadAndServers payload)
      mPayloadAndServers2 = (\{payload, servers} -> {payload, servers: Set.toUnfoldable servers}) <$> mPayloadAndServers
    in
    CallReply mPayloadAndServers2 state


whereIsWithLoad :: forall payload. (AgentLocations -> Locations payload) -> AgentKey -> Effect (List ServerLoad)
whereIsWithLoad extractMap agentKey =
  Gen.call serverName
  \state@{thisServer, members, agentLocations} ->
    let
      streamLocations = _.byAgentKey $ extractMap agentLocations
      toServerLoad (Server el) = serverLoad <$> Map.lookup el.address members
      mPayloadAndServers2 = Map.lookup agentKey streamLocations :: Maybe (AgentPayloadAndServers payload)

      serverList = maybe nil (_.servers >>> Set.toUnfoldable) mPayloadAndServers2
      withLoad = filterMap toServerLoad serverList
    in
    CallReply withLoad state

toLocalOrRemote :: forall a b r1 r2. Newtype a { address :: ServerAddress | r1 } => Newtype b { address :: ServerAddress | r2 } => a -> b -> LocalOrRemote b
toLocalOrRemote thisServer server =
  if extractAddress thisServer == extractAddress server
  then Local server
  else Remote server

getThisIdleServer :: ServerSelectionPredicate -> Effect (Either ResourceFailed Server)
getThisIdleServer pred = Gen.doCall serverName
  (\state@{thisServer, load, acceptingRequests} -> do
      let
        thisServerLoad = toServerLoad thisServer load acceptingRequests
      pure $ if LoadTypes.canLaunch $ pred thisServerLoad
             then CallReply (Right $ thisServer) state
             else CallReply (Left NoCapacity) state
  )

--------------------------------------------------------------------------------
-- Using choice-of-two algorithm, as described in
-- https://www.nginx.com/blog/nginx-power-of-two-choices-load-balancing-algorithm/
getIdleServer :: ServerSelectionPredicate -> Effect (ResourceResp Server)
getIdleServer pred = Gen.doCall serverName
  (\state@{thisServer, members, load, acceptingRequests} -> do
      let
        thisServerLoad = toServerLoad thisServer load acceptingRequests
      if LoadTypes.canLaunch $ pred thisServerLoad
      then pure $ CallReply (Right $ Local thisServer) state
      else do
        let
          candidates = values members
                       # filterMap (\memberInfo@{server} ->
                                      let
                                        serverWithLoad = serverLoad memberInfo
                                        loadCheckResult = pred serverWithLoad
                                      in
                                        if LoadTypes.canLaunch loadCheckResult then
                                          Just (Tuple server loadCheckResult)
                                        else
                                           Nothing
                                   )
        resp <- selectCandidate candidates
        pure $ CallReply resp state
  )

selectCandidate :: List (Tuple Server LoadCheckResult) -> Effect (ResourceResp Server)
selectCandidate candidates =
  let
    numCandidates = length candidates
  in
    case numCandidates of
      0 ->
        pure $ Left NoCapacity
      1 ->
        pure $ head candidates
      _ -> do
        indexes <- distinctRandomNumbers 1 (numCandidates - 1)
        let
          choices = traverse (\i -> index candidates i) indexes
                    # fromMaybe nil
                    # sortBy (\(Tuple _server1 loadCheckResult1) (Tuple _server2 loadCheckResult2) -> compare loadCheckResult1 loadCheckResult2)
        pure $ head choices
  where
    head list = case uncons list of
                  Just {head: (Tuple serverWithLoad _loadCheckResult)} -> Right $ Remote serverWithLoad
                  Nothing -> Left NoCapacity

getCurrentTransPoPLeader :: Effect (Maybe Server)
getCurrentTransPoPLeader =
  Gen.call serverName
    ( \state@{ currentTransPoPLeader: value } ->
        CallReply value state
    )

-- Called by Load to indicate load on this node
announceLoad :: CurrentLoad -> Effect Unit
announceLoad load =
  Gen.doCast serverName
    \state@{ thisServer, members, acceptingRequests } -> do
      let
        thisNodeAddress = extractAddress thisServer
        newMembers = alter (map (\ memberInfo -> memberInfo { load = load })) thisNodeAddress members
      sendToIntraSerfNetwork state $ IMServerLoad thisNodeAddress load acceptingRequests
      pure $ Gen.CastNoReply state { members = newMembers , load = load}

-- Called by LlnwApi to indicate a slot lookup
announceEgestSlotLookup :: RtmpShortName -> SlotName -> SlotLookupResult -> Effect Unit
announceEgestSlotLookup accountName slotName slotLookupResult =
  Gen.doCast serverName
    \state@{ thisServer, members, acceptingRequests } -> do
      let
        thisNodeAddress = extractAddress thisServer
      sendToIntraSerfNetwork state $ IMSlotLookup thisNodeAddress accountName slotName slotLookupResult
      pure $ Gen.CastNoReply state


-- Called by RunState to indicate runState on this node
announceAcceptingRequests :: AcceptingRequests -> Effect Unit
announceAcceptingRequests acceptingRequests =
  Gen.doCast serverName
    \state@{ thisServer, members, load } -> do
      let
        thisNodeAddress = extractAddress thisServer
        newMembers = alter (map (\ memberInfo -> memberInfo { acceptingRequests = acceptingRequests })) thisNodeAddress members
      sendToIntraSerfNetwork state $ IMServerLoad thisNodeAddress load acceptingRequests
      pure $ Gen.CastNoReply state { members = newMembers , acceptingRequests = acceptingRequests}

type AgentMessageHandlerWithSerfPayload a = State -> AgentKey -> Server -> a -> Effect Unit
type AgentMessageHandler = State -> AgentKey -> Server -> Effect Unit

type SetAgentClock = AgentClock -> AgentClocks -> AgentClocks
type AgentClockLens = { get :: AgentClocks -> AgentClock
                      , set :: AgentClock -> AgentClocks -> AgentClocks
                      }

type AgentLocationLens payload = { get :: AgentLocations -> Locations payload
                                 , set :: Locations payload -> AgentLocations -> AgentLocations
                                 }
newtype HandlerName = HandlerName String
derive instance newtypeHandlerName :: Newtype HandlerName _

type AgentHandler payload =
  { name                     :: HandlerName

  , availableLocal           :: AgentMessageHandlerWithSerfPayload payload
  , availableThisPoP         :: AgentMessageHandlerWithSerfPayload payload
  , availableOtherPoP        :: AgentMessageHandlerWithSerfPayload payload
  , stoppedLocal             :: AgentMessageHandler
  , stoppedThisPoP           :: AgentMessageHandler
  , stoppedOtherPoP_viaTrans :: AgentMessageHandler
  , stoppedOtherPoP_viaIntra :: AgentMessageHandler
  , gcThisPoP                :: AgentMessageHandler
  , gcOtherPoP               :: AgentMessageHandler

  , clockLens                :: AgentClockLens
  , locationLens             :: AgentLocationLens payload

  , reannounceEveryMs        :: (State -> Milliseconds)
    -- TODO -- Maybe add isSingleton - to generate warnings if the sets have more than one entry?
  }


type SetServerClock = ServerClock -> ServerClocks -> ServerClocks
type ServerClockLens = { get :: ServerClocks -> ServerClock
                       , set :: ServerClock -> ServerClocks -> ServerClocks
                       }

type ServerMessageHandler =
  { name          :: HandlerName
  , clockLens     :: ServerClockLens
  , handleMessage :: Server -> State -> Effect State
  }

slotLookupHandler :: RtmpShortName -> SlotName -> SlotLookupResult -> ServerMessageHandler
slotLookupHandler rtmpShortName slotName slotLookupResult =
  { name : HandlerName "slotLookup"
  , clockLens: clockLens
  , handleMessage: handleMessage
  }
  where
    clockLens = { get : _.slotLookupClocks
                , set : \newClocks serverClocks -> serverClocks{slotLookupClocks = newClocks}
                }

    handleMessage :: Server -> State -> Effect State
    handleMessage server state@{llnwApiRecordSlotLookupApi} = do
      llnwApiRecordSlotLookupApi rtmpShortName slotName slotLookupResult
      pure state

vmLivenessHandler :: Ref -> ServerMessageHandler
vmLivenessHandler ref =
  { name : HandlerName "vmLiveness"
  , clockLens : clockLens
  , handleMessage : handleMessage
  }
  where
    clockLens = { get : _.vmLivenessClocks
                , set : \newClocks serverClocks -> serverClocks{vmLivenessClocks = newClocks}
                }

    handleMessage :: Server -> State -> Effect State
    handleMessage server state = do
      newState <- case EMap.lookup server state.serverRefs of
        Nothing ->
          pure state
        Just curRef
          | ref == curRef ->
            pure state
          | otherwise ->
            garbageCollectServer state server curRef (Just ref)
      newServerRefs <- EMap.insert' server ref state.serverRefs
      pure newState { serverRefs = newServerRefs }

popLeaderHandler :: ServerMessageHandler
popLeaderHandler =
  { name : HandlerName "popLeader"
  , clockLens : clockLens
  , handleMessage : handleMessage
  }
  where
    clockLens = { get : _.popLeaderClocks
                , set : \newClocks serverClocks -> serverClocks{popLeaderClocks = newClocks}
                }

    handleMessage server state = do
      state.transPoPApi.handleRemoteLeaderAnnouncement server
      pure state{ currentTransPoPLeader = Just server }

loadHandler :: CurrentLoad -> AcceptingRequests -> ServerMessageHandler
loadHandler load acceptingRequests =
  { name : HandlerName "load"
  , clockLens : clockLens
  , handleMessage : handleMessage
  }
  where
    clockLens = { get : _.loadClocks
                , set : \newClocks serverClocks -> serverClocks{loadClocks = newClocks}
                }

    handleMessage server state = do
      let
        newMembers = alter (map (\ memberInfo -> memberInfo { load = load
                                                            , acceptingRequests = acceptingRequests
                                                            })) (extractAddress server) state.members
      pure state{ members = newMembers }


aggregatorHandler :: AgentHandler SlotCharacteristics
aggregatorHandler
  = { name                     : HandlerName "aggregator"
    , availableLocal           : availableLocal
    , availableThisPoP         : availableThisPoP
    , availableOtherPoP        : availableOtherPoP
    , stoppedLocal             : stoppedLocal
    , stoppedThisPoP           : stoppedThisPoP
    , stoppedOtherPoP_viaTrans : stoppedOtherPoP_viaTrans
    , stoppedOtherPoP_viaIntra : stoppedOtherPoP_viaIntra
    , gcThisPoP                : gcThisPoP
    , gcOtherPoP               : gcOtherPoP

    , clockLens                : clockLens
    , locationLens             : locationLens

    , reannounceEveryMs        : _.config >>> _.reannounceAgentEveryMs >>> _.aggregator >>> Long.fromInt >>> Milliseconds
    }
  where
    availableLocal :: AgentMessageHandlerWithSerfPayload SlotCharacteristics
    availableLocal state agentKey server serfPayload = do
      let
        aggregatorKey = agentKeyToAggregatorKey agentKey
      Bus.raise bus (IngestAggregatorStarted aggregatorKey server)
      sendToIntraSerfNetwork state $ IMAggregatorState Available agentKey (extractAddress server) serfPayload
      state.transPoPApi.announceAggregatorIsAvailable serfPayload agentKey server

    availableThisPoP :: AgentMessageHandlerWithSerfPayload SlotCharacteristics
    availableThisPoP state agentKey server serfPayload = do
      let
        aggregatorKey = agentKeyToAggregatorKey agentKey
      Bus.raise bus (IngestAggregatorStarted aggregatorKey server)
      state.transPoPApi.announceAggregatorIsAvailable serfPayload agentKey server

    availableOtherPoP :: AgentMessageHandlerWithSerfPayload SlotCharacteristics
    availableOtherPoP state agentKey server serfPayload = do
      let
        aggregatorKey = agentKeyToAggregatorKey agentKey
      Bus.raise bus (IngestAggregatorStarted aggregatorKey server)
      sendToIntraSerfNetwork state $ IMAggregatorState Available agentKey (extractAddress server) serfPayload

    stoppedLocal :: AgentMessageHandler
    stoppedLocal state agentKey server = do
      let
        aggregatorKey = agentKeyToAggregatorKey agentKey
      Bus.raise bus (IngestAggregatorExited aggregatorKey server)
      sendToIntraSerfNetwork state $ IMAggregatorState Stopped agentKey (extractAddress server) emptySlotCharacteristics
      state.transPoPApi.announceAggregatorStopped agentKey server

    stoppedThisPoP :: AgentMessageHandler
    stoppedThisPoP state agentKey server = do
      let
        aggregatorKey = agentKeyToAggregatorKey agentKey
      logInfo "Remote aggregator stopped in this PoP" {agentKey, server}
      Bus.raise bus (IngestAggregatorExited aggregatorKey server)
      state.transPoPApi.announceAggregatorStopped agentKey server

    stoppedOtherPoP_viaTrans :: AgentMessageHandler
    stoppedOtherPoP_viaTrans state agentKey server = do
      logInfo "Remote aggregator stopped in another PoP" {agentKey, server}
      Bus.raise bus (IngestAggregatorExited (agentKeyToAggregatorKey agentKey)  server)
      sendToIntraSerfNetwork state $ IMAggregatorState Stopped agentKey (extractAddress server) emptySlotCharacteristics

    stoppedOtherPoP_viaIntra :: AgentMessageHandler
    stoppedOtherPoP_viaIntra state agentKey server = do
      logInfo "Remote aggregator stopped in another PoP" {agentKey, server}
      Bus.raise bus (IngestAggregatorExited (agentKeyToAggregatorKey agentKey)  server)

    gcThisPoP :: AgentMessageHandler
    gcThisPoP state agentKey server = do
      logWarning "Remote aggregator timed out" {agentKey, server}
      Bus.raise bus (IngestAggregatorExited (agentKeyToAggregatorKey agentKey) server)
      -- we don't relay timeout messages - should only happen with stressed network anyway...

    gcOtherPoP :: AgentMessageHandler
    gcOtherPoP state agentKey server = do
      logWarning "Aggregator from other pop timed out" {agentKey, server}
      Bus.raise bus (IngestAggregatorExited (agentKeyToAggregatorKey agentKey) server)
      -- we don't relay timeout messages - should only happen with stressed network anyway...



    clockLens = { get : _.aggregatorClocks
                 , set : \newClocks agentClocks -> agentClocks{aggregatorClocks = newClocks}
                 }

    locationLens = { get : _.aggregators
                   , set : \newLocations state -> state {aggregators = newLocations}
                   }

-- Called by IngestAggregator to indicate aggregator start / stop on this node
announceLocalAggregatorIsAvailable :: AggregatorKey -> SlotCharacteristics -> Effect Unit
announceLocalAggregatorIsAvailable key serfPayload = announceAvailableLocal aggregatorHandler serfPayload (aggregatorKeyToAgentKey key)

announceLocalAggregatorStopped :: AggregatorKey -> Effect Unit
announceLocalAggregatorStopped = announceStoppedLocal aggregatorHandler <<< aggregatorKeyToAgentKey

-- -- Called by TransPoP to indicate aggregator start / stop on a node in another PoP
announceOtherPoPAggregatorIsAvailable :: SlotCharacteristics -> AgentKey -> Server -> Effect Unit
announceOtherPoPAggregatorIsAvailable serfPayload = announceAvailableOtherPoP aggregatorHandler serfPayload

announceOtherPoPAggregatorStopped :: AgentKey -> Server -> Effect Unit
announceOtherPoPAggregatorStopped = announceStoppedOtherPoP aggregatorHandler

egestHandler :: AgentHandler Unit
egestHandler
  = { name                     : HandlerName "egest"
    , availableLocal           : availableLocal
    , availableThisPoP         : availableThisPoP
    , availableOtherPoP        : availableOtherPoP
    , stoppedLocal             : stoppedLocal
    , stoppedThisPoP           : stoppedThisPoP
    , stoppedOtherPoP_viaTrans : stoppedOtherPoP
    , stoppedOtherPoP_viaIntra : stoppedOtherPoP
    , gcThisPoP                : gcThisPoP
    , gcOtherPoP               : gcOtherPoP

    , clockLens                : clockLens
    , locationLens             : locationLens

    , reannounceEveryMs        : _.config >>> _.reannounceAgentEveryMs >>> _.egest >>> Long.fromInt >>> Milliseconds

    }
  where
    availableLocal state agentKey server _ = do
      let
        egestKey = agentKeyToEgestKey agentKey
      Bus.raise bus (EgestStarted egestKey server)
      sendToIntraSerfNetwork state $ IMEgestState Available agentKey $ extractAddress server

    availableThisPoP state agentKey server _ = do
      -- logInfo "New egest is avaiable in this PoP" {agentKey, server}
      let
        egestKey = agentKeyToEgestKey agentKey
      Bus.raise bus (EgestStarted egestKey server)
      pure unit

    availableOtherPoP state agentKey server _ = do
      -- Not expecting any of these
      logWarning "New egest is available in another PoP" {agentKey, server}

    stoppedLocal state agentKey server = do
      logInfo "Local egest stopped" {agentKey}
      let
        egestKey = agentKeyToEgestKey agentKey
      Bus.raise bus (EgestExited egestKey server)
      sendToIntraSerfNetwork state $ IMEgestState Stopped agentKey $ extractAddress server

    stoppedThisPoP state agentKey server = do
      let
        egestKey = agentKeyToEgestKey agentKey
      Bus.raise bus (EgestExited egestKey server)
      logInfo "Remote egest stopped" {agentKey, server}

    stoppedOtherPoP state agentKey server = do
      -- Not expecting any of these
      logWarning "Egest stopped in another PoP" {agentKey, server}

    gcThisPoP state agentKey server = do
      logWarning "Remote egest timed out" {agentKey, server}

    gcOtherPoP state agentKey server = do
      -- Not expecting any of these
      logWarning "Egest from other pop timed out" {agentKey, server}

    clockLens = { get : _.egestClocks
                 , set : \newClocks agentClocks -> agentClocks{egestClocks = newClocks}
                 }

    locationLens = { get : _.egests
                    , set : \newLocations state -> state {egests = newLocations}
                    }



-- Called by EgestAgent to indicate egest on this node
announceLocalEgestIsAvailable :: EgestKey -> Effect Unit
announceLocalEgestIsAvailable = announceAvailableLocal egestHandler unit <<< egestKeyToAgentKey

announceLocalEgestStopped :: EgestKey -> Effect Unit
announceLocalEgestStopped = announceStoppedLocal egestHandler <<< egestKeyToAgentKey

relayHandler :: AgentHandler Unit
relayHandler
  = { name                     : HandlerName "relay"
    , availableLocal           : availableLocal
    , availableThisPoP         : availableThisPoP
    , availableOtherPoP        : availableOtherPoP
    , stoppedLocal             : stoppedLocal
    , stoppedThisPoP           : stoppedThisPoP
    , stoppedOtherPoP_viaTrans : stoppedOtherPoP
    , stoppedOtherPoP_viaIntra : stoppedOtherPoP
    , gcThisPoP                : gcThisPoP
    , gcOtherPoP               : gcOtherPoP

    , clockLens                : clockLens
    , locationLens             : locationLens

    , reannounceEveryMs        : _.config >>> _.reannounceAgentEveryMs >>> _.relay >>> Long.fromInt >>> Milliseconds
    }
  where
    availableLocal state agentKey server _ = do
      let
        relayKey = agentKeyToRelayKey agentKey
      Bus.raise bus (StreamRelayStarted relayKey server)
      sendToIntraSerfNetwork state $ IMRelayState Available agentKey $ extractAddress server

    availableThisPoP state agentKey server _ = do
      let
        relayKey = agentKeyToRelayKey agentKey
      Bus.raise bus $ StreamRelayStarted relayKey server
      pure unit

    availableOtherPoP state agentKey server _ = do
      -- Not expecting any of these
      logWarning "New relay is available in another PoP" {agentKey, server}

    stoppedLocal state agentKey server = do
      logInfo "Local relay stopped" {agentKey}
      let
        relayKey = agentKeyToRelayKey agentKey
      Bus.raise bus $ StreamRelayExited relayKey server
      sendToIntraSerfNetwork state $ IMRelayState Stopped agentKey $ extractAddress server

    stoppedThisPoP state agentKey server = do
      let
        relayKey = agentKeyToRelayKey agentKey
      Bus.raise bus (StreamRelayExited relayKey server)
      logInfo "Remote relay stopped" {agentKey, server}

    stoppedOtherPoP state agentKey server = do
      -- Not expecting any of these
      logWarning "Relay stopped in another PoP" {agentKey, server}

    gcThisPoP state agentKey server = do
      logWarning "Remote relay timed out" {agentKey, server}

    gcOtherPoP state agentKey server = do
      -- Not expecting any of these
      logWarning "Relay from other pop timed out" {agentKey, server}


    clockLens = { get : _.relayClocks
                 , set : \newClocks agentClocks -> agentClocks{relayClocks = newClocks}
                 }

    locationLens = { get : _.relays
                    , set : \newLocations state -> state {relays = newLocations}
                    }

-- Called by RelayAgent to indicate relay on this node
announceLocalRelayIsAvailable :: RelayKey -> Effect Unit
announceLocalRelayIsAvailable (RelayKey slotId slotRole) = do
  announceAvailableLocal relayHandler unit (AgentKey slotId slotRole)

announceLocalRelayStopped :: RelayKey -> Effect Unit
announceLocalRelayStopped (RelayKey slotId slotRole) = do
  announceStoppedLocal relayHandler (AgentKey slotId slotRole)

-- Builds public API for events on this server
announceAvailableLocal :: forall payload. Ord payload => AgentHandler payload -> payload -> AgentKey -> Effect Unit
announceAvailableLocal handler@{locationLens} serfPayload agentKey =
  Gen.doCast serverName
    \state@{thisServer} -> do
      if isNewLocalAgent handler (locationLens.get state.agentLocations) thisServer agentKey then
        doAnnounceAvailableLocal handler serfPayload agentKey state
      else do
        logInfo "not announcing since already exists" {agentKey}
        pure unit
      pure $ Gen.CastNoReply $ updateAgentLocation (recordLocalAgent serfPayload) locationLens agentKey thisServer state

doAnnounceAvailableLocal :: forall a. AgentHandler a -> a -> AgentKey -> State -> Effect Unit
doAnnounceAvailableLocal handler serfPayload agentKey state@{thisServer, agentLocations} = do
  handler.availableLocal state agentKey thisServer serfPayload
  void $ Timer.sendAfter serverName (fromMaybe 0 $ Long.toInt $ unwrap $ handler.reannounceEveryMs state) $ ReAnnounce agentKey handler (doAnnounceAvailableLocal handler serfPayload agentKey)

announceStoppedLocal :: forall a. AgentHandler a -> AgentKey -> Effect Unit
announceStoppedLocal handler@{locationLens} agentKey = do
  Gen.doCast serverName
    \state@{ thisServer } -> do
      logInfo (unwrap handler.name <> " stopped on this node") {agentKey}
      handler.stoppedLocal state agentKey thisServer
      pure $ Gen.CastNoReply $ updateAgentLocation removeLocalAgent locationLens agentKey thisServer state


-- Builds public API for message from other PoP
announceAvailableOtherPoP :: forall payload. Ord payload => AgentHandler payload -> payload ->  AgentKey -> Server -> Effect Unit
announceAvailableOtherPoP handler@{locationLens} serfPayload agentKey msgServer =
  Gen.doCast serverName
    \state@{agentLocations, thisServer} -> do
      let
        locations = locationLens.get agentLocations
      timeout <- messageTimeout handler state
      logIfNewAgent handler locations thisServer agentKey msgServer
      handler.availableOtherPoP state agentKey msgServer serfPayload
      pure $ Gen.CastNoReply $ updateAgentLocation (recordRemoteAgent timeout serfPayload) locationLens agentKey msgServer state

announceStoppedOtherPoP :: forall a. AgentHandler a -> AgentKey -> Server -> Effect Unit
announceStoppedOtherPoP handler@{locationLens} agentKey server =
  Gen.doCast serverName
    \state -> do
      handler.stoppedOtherPoP_viaTrans state agentKey server
      pure $ Gen.CastNoReply $ updateAgentLocation removeRemoteAgent locationLens agentKey server state

-- Called by TransPoP to indicate that it is acting as this PoP's leader
announceTransPoPLeader :: Effect Unit
announceTransPoPLeader =
  Gen.doCast serverName
    \state@{ thisServer, transPoPApi:{handleRemoteLeaderAnnouncement: transPoP_announceTransPoPLeader} } ->
    do
      transPoP_announceTransPoPLeader thisServer
      sendToIntraSerfNetwork state $ IMTransPoPLeader $ extractAddress thisServer
      pure $ Gen.CastNoReply state{currentTransPoPLeader = Just thisServer}


updateAgentLocation :: forall payload. (AgentKey -> Server -> Locations payload -> Locations payload) -> AgentLocationLens payload -> AgentKey -> Server -> State -> State
updateAgentLocation action lens agentKey server state@{agentLocations} =
  let
    locations = lens.get agentLocations
    newLocations = action agentKey server locations
    newAgentLocations = lens.set newLocations agentLocations
  in
    state { agentLocations = newAgentLocations }

--------------------------------------------------------------------------------
-- Gen Server methods
--------------------------------------------------------------------------------
type StartArgs =
  { config :: Config.IntraPoPAgentConfig
  , transPoPApi :: Config.TransPoPAgentApi
  , llnwApiRecordSlotLookupApi :: RtmpShortName -> SlotName -> SlotLookupResult -> Effect Unit
  , canaryState :: CanaryState
  , acceptingRequestsFun :: Effect AcceptingRequests
  }

startLink :: StartArgs -> Effect StartLinkResult
startLink args = Gen.startLink serverName (init args) handleInfo

init :: StartArgs -> Effect State
init { config
     , transPoPApi
     , llnwApiRecordSlotLookupApi
     , canaryState
     , acceptingRequestsFun
     } = do

  logInfo "Intra-PoP Agent Starting" {config: config}
  healthConfig <- Config.healthConfig
  Gen.registerExternalMapping serverName (\m -> IntraPoPSerfMsg <$> (Serf.messageMapper m))

  let
    garbageCollectVMInterval = config.vmLivenessIntervalMs / 2
    garbageCollectAgentInterval = ( hfoldl (min :: Int -> Int -> Int) config.reannounceAgentEveryMs.aggregator
                                     config.reannounceAgentEveryMs
                                 ) / 2

  void $ Timer.sendAfter serverName 0 (GetAcceptingRequests acceptingRequestsFun)
  void $ Timer.sendAfter serverName 0 JoinAll
  void $ Timer.sendEvery serverName config.rejoinEveryMs JoinAll
  void $ Timer.sendEvery serverName config.vmLivenessIntervalMs VMLiveness
  void $ Timer.sendEvery serverName garbageCollectVMInterval GarbageCollectVM
  void $ Timer.sendEvery serverName garbageCollectAgentInterval GarbageCollectAgents
  rpcBindIp <- Env.intraSerfIp
  thisServer <- PoPDefinition.getThisServer
  let
    serfRpcAddress =
      { ip: show rpcBindIp
      , port: config.rpcPort
      }
  membersResp <- Serf.members serfRpcAddress
  streamResp <- Serf.stream serfRpcAddress
  case streamResp of
    Left error -> do
      logInfo "Could not connect to IntraPoP Serf Agent" { error: error }
      Erl.sleep $ Milliseconds $ Long.fromInt 100 -- Just so we don't spin like crazy...
      unsafeCrashWith ("could_not_connect_stream")
    Right _ ->
      pure unit

  serversInPoP <- PoPDefinition.serversInThisPoPByAddress
  thisServerRef <- makeRef

  let
    emptyLocations :: forall payload. Locations payload
    emptyLocations = { byAgentKey       : Map.empty
                     , byRemoteServer   : Map.empty
                     , remoteTimeouts   : Map.empty
                     }
    members = membersResp
              # hush
              # fromMaybe nil
              # filterMap (\member@{name} ->
                            let
                              serverAddress = ServerAddress name
                              serverToMemberInfo server =
                                { server: server
                                , load: maxLoad
                                , acceptingRequests: wrap false
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
    , llnwApiRecordSlotLookupApi
    , serfRpcAddress
    , currentTransPoPLeader: Nothing

    , testDropAgentMessages: false

    , thisServerRef
    , serverRefs: EMap.empty

    , thisServer: thisServer
    , load: minLoad
    , acceptingRequests: wrap false
    , canary: canaryState
    , members
    , agentClocks  : { aggregatorClocks: Map.empty
                     , egestClocks: Map.empty
                     , relayClocks: Map.empty
                     }
    , agentLocations : { relays: emptyLocations
                       , aggregators:  emptyLocations
                       , egests :  emptyLocations
                       }

    , serverClocks: { loadClocks: Map.empty
                    , popLeaderClocks: Map.empty
                    , vmLivenessClocks : Map.empty
                    , slotLookupClocks : Map.empty
                    }
    }

handleInfo :: forall serfPayload. Msg serfPayload -> State -> Effect (CastResult State)
handleInfo msg state = case msg of

  IntraPoPSerfMsg imsg ->
    CastNoReply <$> handleIntraPoPSerfMsg imsg state

  GetAcceptingRequests acceptingRequestsFun -> do
    acceptingRequests <- acceptingRequestsFun
    pure $ CastNoReply state{acceptingRequests = acceptingRequests}

  JoinAll -> do
    joinAllSerf state
    pure $ CastNoReply state

  GarbageCollectVM ->
    CastNoReply <$> garbageCollectVM state

  GarbageCollectAgents ->
    CastNoReply <$> garbageCollectAgents state

  VMLiveness -> do
    sendToIntraSerfNetwork state $ IMVMLiveness (extractAddress state.thisServer) state.thisServerRef
    pure $ CastNoReply state

  ReAnnounce agentKey handler announceFun -> do
    CastNoReply <$> maybeReannounce
    where
      maybeReannounce :: Effect State
      maybeReannounce = do
        let
          -- Do we still know about the asset? If we do, rebroadcast its existence and set up another timer for next time
          locations = handler.locationLens.get state.agentLocations
        if mapSetMember agentKey state.thisServer locations.byAgentKey
        then do
          announceFun state
          pure state
        else
          pure state

      mapSetMember key server mapSet = fromMaybe false $ Set.member server <$> _.servers <$> Map.lookup key mapSet



handleIntraPoPSerfMsg :: (Serf.SerfMessage IntraMessage) -> State -> Effect State
handleIntraPoPSerfMsg imsg state@{ transPoPApi: {handleRemoteLeaderAnnouncement}
                                 , thisServer
                                 , serverClocks
                                 , agentClocks
                                 } =
  case imsg of
    Serf.MemberAlive members -> membersAlive members state
    Serf.MemberLeaving -> pure state
    Serf.MemberLeft members -> membersLeft members state
    Serf.MemberFailed -> pure state

    Serf.StreamFailed -> do
      logInfo "Lost connection to IntraPoP Serf Agent" {}
      Erl.sleep $ Milliseconds $ Long.fromInt 100 -- Just so we don't spin like crazy...
      -- TODO send a "stepping down message?" - except without Serf I guess we can't
      unsafeCrashWith ("lost_serf_connection")

    Serf.UserEvent name ltime coalesce intraMessage -> do
      case intraMessage of
        IMAggregatorState eventType agentKey msgOrigin serfPayload -> do
          handleAgentMessage ltime eventType agentKey msgOrigin state aggregatorHandler serfPayload

        IMEgestState eventType agentKey msgOrigin -> do
          handleAgentMessage ltime eventType agentKey msgOrigin state egestHandler unit

        IMRelayState eventType agentKey msgOrigin -> do
          handleAgentMessage ltime eventType agentKey msgOrigin state relayHandler unit

        IMServerLoad msgOrigin load acceptingRequests -> do
          handleServerMessage ltime msgOrigin state $ loadHandler load acceptingRequests

        IMTransPoPLeader msgOrigin -> do
          handleServerMessage ltime msgOrigin state popLeaderHandler

        IMVMLiveness msgOrigin ref -> do
          handleServerMessage ltime msgOrigin state $ vmLivenessHandler ref

        IMSlotLookup msgOrigin rtmpShortName slotName slotLookupResult -> do
          handleServerMessage ltime msgOrigin state $ slotLookupHandler rtmpShortName slotName slotLookupResult

handleServerMessage :: LamportClock -> ServerAddress -> State -> ServerMessageHandler -> Effect State
handleServerMessage msgLTime msgServerAddress
                    state@{thisServer, serverClocks}
                    serverMessageHandler@{clockLens, handleMessage} = do
  -- let _ = spy "serverMessage" {name: serverMessageHandler.name, msgServerAddress}
  -- Make sure the message is from a known origin and does not have an expired Lamport clock
  if msgServerAddress == extractAddress thisServer
  then
    pure state
  else let serverClock = clockLens.get state.serverClocks in
    if Map.lookup msgServerAddress serverClock # maybe false (_ >= msgLTime)
    then
      pure state
    else do
      -- TODO - maybe cache the db for a while to prevent hot calls, or use ETS etc
      mServer <- PoPDefinition.whereIsServer msgServerAddress
      case mServer of
        Nothing -> do
          logWarning "message from unknown server" {msgServerAddress, msgType: serverMessageHandler.name}
          pure state
        Just msgServer -> do
          let
            newServerClock = Map.insert msgServerAddress msgLTime serverClock
            newServerClocks = clockLens.set newServerClock state.serverClocks
          handleMessage msgServer state{ serverClocks = newServerClocks}


handleAgentMessage :: forall payload. Ord payload => LamportClock -> EventType -> AgentKey -> ServerAddress -> State -> AgentHandler payload -> payload -> Effect State
handleAgentMessage msgLTime eventType agentKey msgServerAddress
                  state@{thisServer, agentClocks, agentLocations}
                  agentMessageHandler@{clockLens, locationLens} serfPayload = do
  -- let _ = spy "agentMessage" {name: agentMessageHandler.name, eventType, agentKey, msgServerAddress}
  -- Make sure the message is from a known origin and does not have an expired Lamport clock
  if msgServerAddress == extractAddress thisServer || state.testDropAgentMessages
  then
    pure state
  else let agentClock = clockLens.get state.agentClocks in
    if Map.lookup (Tuple msgServerAddress agentKey) agentClock # maybe false (_ >= msgLTime)
    then
      pure state
    else do
      -- TODO - maybe cache the db for a while to prevent hot calls, or use ETS etc
      mServer <- PoPDefinition.whereIsServer msgServerAddress
      case mServer of
        Nothing -> do
          logWarning "message from unknown server" {msgServerAddress, msgType: agentMessageHandler.name}
          pure state
        Just msgServer -> do
          let
            fromThisPoP = extractPoP msgServer == extractPoP state.thisServer
            newAgentClock = Map.insert (Tuple msgServerAddress agentKey) msgLTime agentClock
            newAgentClocks = clockLens.set newAgentClock state.agentClocks
            locations = locationLens.get agentLocations

          case eventType of
            Available -> do
              timeout <- messageTimeout agentMessageHandler state
              logIfNewAgent agentMessageHandler locations thisServer agentKey msgServer
              let
                newLocations = recordRemoteAgent timeout serfPayload agentKey msgServer locations
              if fromThisPoP
              then agentMessageHandler.availableThisPoP state agentKey msgServer serfPayload
              else pure unit
              pure $ state { agentClocks = newAgentClocks
                           , agentLocations = locationLens.set newLocations agentLocations
                           }

            Stopped -> do
              let
                newLocations = removeRemoteAgent agentKey msgServer locations
              if fromThisPoP
              then agentMessageHandler.stoppedThisPoP state agentKey msgServer
              else agentMessageHandler.stoppedOtherPoP_viaIntra state agentKey msgServer
              pure $ state { agentClocks = newAgentClocks
                           , agentLocations = locationLens.set newLocations agentLocations
                           }


messageTimeout :: forall a. AgentHandler a -> State -> Effect Milliseconds
messageTimeout agentMessageHandler state = do
  now <- Erl.systemTimeMs
  pure $ now + (agentMessageHandler.reannounceEveryMs state) * (Milliseconds $ Long.fromInt state.config.missCountBeforeExpiry)

--------------------------------------------------------------------------------
-- Internal functions
--------------------------------------------------------------------------------
serverName :: forall serfPayload. ServerName State (Msg serfPayload)
serverName = Names.intraPoPName

isNewLocalAgent :: forall payload. AgentHandler payload -> Locations payload -> Server -> AgentKey -> Boolean
isNewLocalAgent handler locations thisServer agentKey =
  case Map.lookup agentKey locations.byAgentKey of
    Nothing -> true
    Just {servers} -> not $ Set.member thisServer servers

logIfNewAgent :: forall payload. AgentHandler payload -> Locations payload -> Server -> AgentKey -> Server -> Effect Unit
logIfNewAgent handler locations thisServer agentKey agentServer =
  if Map.member (Tuple agentKey agentServer) locations.remoteTimeouts
  then pure unit
  else do
    let
      scope = if extractPoP thisServer == extractPoP agentServer
              then "in this PoP"
              else "in another PoP"
    logInfo ("New " <> unwrap handler.name <> " is available " <> scope) {agentKey, agentServer}

sendToIntraSerfNetwork :: State -> IntraMessage -> Effect Unit
sendToIntraSerfNetwork {canary: Canary} _msg = pure unit
sendToIntraSerfNetwork state msg = do
  result <- Serf.event state.serfRpcAddress msg false
  maybeLogError "Intra-PoP serf event failed" result {msg}


membersAlive :: (List Serf.SerfMember) -> State -> Effect State
membersAlive members state = do
  logInfo "Members Alive" { members: _.name <$> members }

  serversInPoP <- PoPDefinition.serversInThisPoPByAddress

  let
    makeMemberInfo member@{ name } server =
      { serfMember: member
      , server: server
      , load: maxLoad
      , acceptingRequests: wrap false
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

garbageCollectVM :: State -> Effect State
garbageCollectVM state@{ config
                       , serverRefs
                       } =
  do
    now <- Erl.systemTimeMs
    let
      expireThreshold = Milliseconds $ Long.fromInt $ config.missCountBeforeExpiry * config.vmLivenessIntervalMs
      threshold = now - expireThreshold
      Tuple newServerRefs garbage = EMap.garbageCollect2 threshold serverRefs
      garbageCollectServer' s (Tuple server ref) = garbageCollectServer s server ref Nothing
    foldM garbageCollectServer' state{serverRefs = newServerRefs} garbage


garbageCollectAgents :: State -> Effect State
garbageCollectAgents state = do
  now <- Erl.systemTimeMs
  foldHandlers (gcAgent now) state

gcAgent :: forall a. Milliseconds -> AgentHandler a -> State -> Effect State
gcAgent now handler@{locationLens} state = do
  let
    thisPoP = extractPoP state.thisServer
    locations = locationLens.get state.agentLocations
    garbage = foldlWithIndex expired nil locations.remoteTimeouts
  foldM (gc thisPoP) state garbage
--  pure $ locationLens.set newLocations state
  --key@(Tuple agentKey server) acc v = if v < now then (key : acc) else acc
  where
    expired k acc v = if v < now then (k : acc) else acc
    gc thisPoP state' (Tuple agentKey server) = do
      if extractPoP server == thisPoP
      then handler.gcThisPoP state' agentKey server
      else handler.gcOtherPoP state' agentKey server
      pure $ updateAgentLocation removeRemoteAgent handler.locationLens agentKey server state'


foldHandlers :: forall acc. (forall a. AgentHandler a -> acc -> Effect acc) -> acc -> Effect acc
foldHandlers perHandlerFun =
  perHandlerFun relayHandler >=> perHandlerFun aggregatorHandler >=> perHandlerFun egestHandler

garbageCollectServer :: State -> Server -> Ref -> Maybe Ref -> Effect State
garbageCollectServer state@{members} deadServer oldRef newRef = do
  -- TODO - add a server decomissioning message to do this cleanly
  logWarning "server liveness timeout" { server: deadServer
                                       , oldRef
                                       , newRef}
  Bus.raise bus (VmReset deadServer oldRef newRef)
  let
    newMembers = alter (map (\ memberInfo -> memberInfo { acceptingRequests = wrap false })) (extractAddress deadServer) members
  foldHandlers (gcServer deadServer) state{members = newMembers}

gcServer :: forall a. Server -> AgentHandler a -> State -> Effect State
gcServer deadServer agentHandler@{locationLens, name, stoppedThisPoP} state@{agentLocations} = do
  let
    locations = locationLens.get agentLocations
  case Map.lookup deadServer locations.byRemoteServer of
    Nothing -> pure state
    Just garbageStreams -> do
      logWarning (unwrap name <> " liveness timeout") {garbageStreams}
      newByAgentKey <- foldM cleanUp locations.byAgentKey  garbageStreams
      let
        newTimeouts = foldl (\acc k -> Map.delete (Tuple k deadServer) acc) locations.remoteTimeouts garbageStreams
        newLocations = { byAgentKey : newByAgentKey
                       , byRemoteServer : Map.delete deadServer locations.byRemoteServer
                       , remoteTimeouts : newTimeouts
                       }
      pure $ state {agentLocations = locationLens.set newLocations agentLocations}
  where
    cleanUp acc agentKey = do
      stoppedThisPoP state agentKey deadServer
      pure $ mapSetDeleteWithPayload agentKey deadServer acc



recordLocalAgent :: forall payload. Ord payload =>  payload-> AgentKey -> Server -> Locations payload -> Locations payload
recordLocalAgent payload agentKey server locations =
  locations { byAgentKey = mapSetInsertWithPayload agentKey payload server locations.byAgentKey
            }

removeLocalAgent :: forall payload. AgentKey -> Server -> Locations payload -> Locations payload
removeLocalAgent agentKey thisServer locations =
  locations { byAgentKey = mapSetDeleteWithPayload agentKey thisServer locations.byAgentKey
            }

recordRemoteAgent :: forall payload. Ord payload => Milliseconds -> payload -> AgentKey -> Server -> Locations payload -> Locations payload
recordRemoteAgent timeout payload agentKey server locations =
  { byAgentKey     : mapSetInsertWithPayload agentKey payload server locations.byAgentKey
  , byRemoteServer : mapSetInsert server agentKey locations.byRemoteServer
  , remoteTimeouts : Map.insert (Tuple agentKey server) timeout locations.remoteTimeouts
  }

removeRemoteAgent :: forall payload. AgentKey -> Server -> Locations payload -> Locations payload
removeRemoteAgent agentKey server locations =
  { byAgentKey     : mapSetDeleteWithPayload agentKey server locations.byAgentKey
  , byRemoteServer : mapSetDelete server agentKey locations.byRemoteServer
  , remoteTimeouts : Map.delete(Tuple agentKey server) locations.remoteTimeouts
  }

mapSetInsertWithPayload :: forall payload. AgentKey -> payload -> Server -> Map AgentKey (AgentPayloadAndServers payload) -> Map AgentKey (AgentPayloadAndServers payload)
mapSetInsertWithPayload key payload server mapSet =
  let
    {servers} = fromMaybe {payload, servers: Set.empty} $ Map.lookup key mapSet
  in
    Map.insert key {payload, servers: Set.insert server servers} mapSet

mapSetDeleteWithPayload :: forall payload. AgentKey -> Server -> Map AgentKey (AgentPayloadAndServers payload) -> Map AgentKey (AgentPayloadAndServers payload)
mapSetDeleteWithPayload key server mapSet =
  case Map.lookup key mapSet of
    Nothing -> mapSet
    Just {payload, servers} ->
      let
        newServers = Set.delete server servers
      in
       if Set.isEmpty newServers
       then Map.delete key mapSet
       else Map.insert key {payload, servers: newServers} mapSet

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
        toJoin = Map.keys $ Map.difference (toMap (unwrap >>> _.address <$> allOtherServers)) members
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

                  maybeLogError "Intra-PoP serf join failed" result {}
                  pure unit
              )
        traverse_ joinAsync toJoin
  where
  toMap :: forall a. List a -> Map a Unit
  toMap list = foldl (\acc item -> Map.insert item unit acc) Map.empty list

serverLoad :: MemberInfo -> ServerLoad
serverLoad {server, load, acceptingRequests} = toServerLoad server load acceptingRequests

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
