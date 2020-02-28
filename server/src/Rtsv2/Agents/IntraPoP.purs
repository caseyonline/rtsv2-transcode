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
  , announceTransPoPLeader


    -- State queries

  , getPublicState

  , whereIsIngestAggregator
  , whereIsStreamRelay
  , whereIsEgest
  , getIdleServer
  , getCurrentTransPoPLeader

    -- Helper - not sure it's used... -- TODO
  , launchLocalOrRemoteGeneric

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
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Set (Set)
import Data.Set as Set
import Data.Traversable (traverse_)
import Data.Tuple (Tuple(..), fst)
import Effect (Effect)
import Effect.Random (randomInt)
import Ephemeral.Map (EMap)
import Ephemeral.Map as EMap
import Erl.Atom (Atom)
import Erl.Data.List (List, head, index, length, nil, singleton, sortBy, take, uncons, (:))
import Erl.Data.Map (Map, alter, fromFoldable, values)
import Erl.Data.Map as Map
import Erl.Process (Process, spawnLink)
import Erl.Utils (Ref, makeRef)
import Erl.Utils as Erl
import Heterogeneous.Folding (hfoldl)
import Logger (Logger, spy)
import Logger as Logger
import Partial.Unsafe (unsafeCrashWith, unsafePartial)
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
import Shared.Stream (AgentKey(..), AggregatorKey, EgestKey(..), RelayKey(..), SlotRole(..), agentKeyToAggregatorKey, aggregatorKeyToAgentKey)
import Shared.Types (Load, Milliseconds, Server(..), ServerAddress(..), ServerLoad(..), extractAddress, extractPoP, serverLoadToServer, toServer, toServerLoad)
import Shared.Types.Agent.State as PublicState


type TestHelperPayload =
  { dropAgentMessages :: Boolean
  }


type MemberInfo =
  { serfMember :: Serf.SerfMember
  , load :: Load
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
  }


type AgentLocations = { relays                :: Locations
                      , aggregators           :: Locations
                      , egests                :: Locations
                      }


type Locations = { byAgentKey     :: Map AgentKey (Set Server)
                 , byRemoteServer :: Map Server (Set AgentKey)
                 , remoteTimeouts :: Map (Tuple AgentKey Server) Milliseconds
                 }

type State
  = { config                :: Config.IntraPoPAgentConfig
    , healthConfig          :: Config.HealthConfig
    , transPoPApi           :: Config.TransPoPAgentApi
    , serfRpcAddress        :: IpAndPort
    , currentTransPoPLeader :: Maybe Server

    , testDropAgentMessages :: Boolean

    , thisServerRef         :: Ref
    , serverRefs            :: EMap Server Ref

    , thisServer            :: Server
    , load                  :: Load
    , members               :: Map ServerAddress MemberInfo
    , agentClocks           :: AgentClocks
    , agentLocations        :: AgentLocations
    , serverClocks          :: ServerClocks
    }

data EventType
  = Available
  | Stopped

data IntraMessage
  = IMAggregatorState EventType AgentKey ServerAddress
  | IMEgestState EventType AgentKey ServerAddress
  | IMRelayState EventType AgentKey ServerAddress

  | IMServerLoad ServerAddress Load
  | IMTransPoPLeader ServerAddress
  | IMVMLiveness ServerAddress Ref

data Msg
  = JoinAll
  | GarbageCollectVM
  | GarbageCollectAgents
  | IntraPoPSerfMsg (Serf.SerfMessage IntraMessage)
  | VMLiveness
  | ReAnnounce AgentKey HandlerName

data IntraPoPBusMessage =
  IngestAggregatorExited AggregatorKey Server



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


testHelper :: TestHelperPayload -> Effect Unit
testHelper payload =
  Gen.doCall serverName \state -> do
    pure $ CallReply unit state {testDropAgentMessages = payload.dropAgentMessages}



getPublicState :: Effect (PublicState.IntraPoP List)
getPublicState = exposeState publicState serverName
  where
    publicState state@{agentLocations, currentTransPoPLeader} =
      { aggregatorLocations: toAggregatorLocation <$>  Map.toUnfoldable agentLocations.aggregators.byAgentKey
      , relayLocations: toSlotIdAndProfileName <$>  Map.toUnfoldable agentLocations.relays.byAgentKey
      , egestLocations: toSlotIdAndProfileName <$>  Map.toUnfoldable agentLocations.egests.byAgentKey
      , currentTransPoPLeader
      }
    toAggregatorLocation (Tuple (AgentKey slotId role) v) =
      { slotId
      , role
      , servers:  Set.toUnfoldable v
      }
    toSlotIdAndProfileName (Tuple (AgentKey slotId role) v) =
      { slotId
      , role
      , servers:  Set.toUnfoldable v
      }


-- TODO - we should be calling the prime' versions and handling that there might, in fact be more than
-- one instance of things we'd like to be singletons
whereIsIngestAggregator :: AggregatorKey -> Effect (Maybe Server)
whereIsIngestAggregator aggregatorKey = head <$> whereIs (_.aggregators) (aggregatorKeyToAgentKey aggregatorKey)

whereIsStreamRelay :: RelayKey -> Effect (Maybe (LocalOrRemote Server))
whereIsStreamRelay (RelayKey slotId streamRole)  = head <$> (map $ map serverLoadToServer) <$> whereIsStreamRelay' (AgentKey slotId streamRole)

whereIsEgest :: EgestKey -> Effect (List ServerLoad)
whereIsEgest egestKey = (map fromLocalOrRemote) <$> (whereIsEgest' $ egestKeyToAgentKey egestKey)

whereIsStreamRelay' :: AgentKey -> Effect (List (LocalOrRemote ServerLoad))
whereIsStreamRelay' = whereIsWithLoad _.relays

whereIsEgest' :: AgentKey -> Effect (List (LocalOrRemote ServerLoad))
whereIsEgest' = whereIsWithLoad _.egests


whereIs ::(AgentLocations -> Locations) -> AgentKey -> Effect (List Server)
whereIs extractMap agentKey =
  Gen.call serverName
  \state@{thisServer, members, agentLocations} ->
    let
      streamLocations = _.byAgentKey $ extractMap agentLocations
      mLocations = Map.lookup agentKey streamLocations :: Maybe (Set Server)
      locations = maybe nil Set.toUnfoldable mLocations
    in
    CallReply locations state


whereIsWithLoad ::(AgentLocations -> Locations) -> AgentKey -> Effect (List (LocalOrRemote ServerLoad))
whereIsWithLoad extractMap agentKey =
  Gen.call serverName
  \state@{thisServer, members, agentLocations} ->
    let
      streamLocations = _.byAgentKey $ extractMap agentLocations
      withLoad (Server el) = serverLoad <$> Map.lookup el.address members
      mLocations = Map.lookup agentKey streamLocations :: Maybe (Set Server)

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


getCurrentTransPoPLeader :: Effect (Maybe Server)
getCurrentTransPoPLeader =
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
      sendToIntraSerfNetwork state "loadUpdate" (IMServerLoad thisNodeAddress load)
      pure $ Gen.CastNoReply state { members = newMembers , load = load}


type AgentMessageHandler = State -> AgentKey -> Server -> Effect Unit

type SetAgentClock = AgentClock -> AgentClocks -> AgentClocks
type AgentClockLens = { get :: AgentClocks -> AgentClock
                      , set :: AgentClock -> AgentClocks -> AgentClocks
                      }

type AgentLocationLens = { get :: AgentLocations -> Locations
                         , set :: Locations -> AgentLocations -> AgentLocations
                         }
newtype HandlerName = HandlerName String
derive instance newtypeHandlerName :: Newtype HandlerName _

type AgentHandler =
  { name              :: HandlerName

  , availableLocal    :: AgentMessageHandler
  , availableThisPoP  :: AgentMessageHandler
  , availableOtherPoP :: AgentMessageHandler
  , stoppedLocal      :: AgentMessageHandler
  , stoppedThisPoP    :: AgentMessageHandler
  , stoppedOtherPoP   :: AgentMessageHandler
  , gcThisPoP         :: AgentMessageHandler
  , gcOtherPoP        :: AgentMessageHandler

  , clockLens         :: AgentClockLens
  , locationLens      :: AgentLocationLens

  , reannounceEveryMs :: (State -> Milliseconds)
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
            garbageCollectServer state server
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

loadHandler :: Load -> ServerMessageHandler
loadHandler load =
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
        newMembers = alter (map (\ memberInfo -> memberInfo { load = load })) (extractAddress server) state.members
      pure state{ members = newMembers }


aggregatorHandler :: AgentHandler
aggregatorHandler
  = { name              : HandlerName "aggregator"
    , availableLocal    : availableLocal
    , availableThisPoP  : availableThisPoP
    , availableOtherPoP : availableOtherPoP
    , stoppedLocal      : stoppedLocal
    , stoppedThisPoP    : stoppedThisPoP
    , stoppedOtherPoP   : stoppedOtherPoP
    , gcThisPoP         : gcThisPoP
    , gcOtherPoP        : gcOtherPoP

    , clockLens         : clockLens
    , locationLens      : locationLens

    , reannounceEveryMs : _.config >>> _.reannounceAgentEveryMs >>> _.aggregator >>> wrap
    }
  where
    availableLocal :: AgentMessageHandler
    availableLocal state agentKey server = do
      sendToIntraSerfNetwork state "aggregatorAvailable" (IMAggregatorState Available agentKey (extractAddress server))
      state.transPoPApi.announceAggregatorIsAvailable agentKey server

    availableThisPoP :: AgentMessageHandler
    availableThisPoP state agentKey server = do
      state.transPoPApi.announceAggregatorIsAvailable agentKey server

    availableOtherPoP :: AgentMessageHandler
    availableOtherPoP state agentKey server = do
      sendToIntraSerfNetwork state "aggregatorAvailable" (IMAggregatorState Available agentKey (extractAddress server))


    stoppedLocal :: AgentMessageHandler
    stoppedLocal state agentKey server = do
      let
        aggregatorKey = agentKeyToAggregatorKey agentKey
      Bus.raise bus (IngestAggregatorExited aggregatorKey server)
      sendToIntraSerfNetwork state "aggregatorStopped" $ IMAggregatorState Stopped agentKey $ extractAddress server
      state.transPoPApi.announceAggregatorStopped agentKey server

    stoppedThisPoP :: AgentMessageHandler
    stoppedThisPoP state agentKey server = do
      let
        aggregatorKey = agentKeyToAggregatorKey agentKey
      logInfo "Remote aggregator stopped in this PoP" {agentKey, server}
      Bus.raise bus (IngestAggregatorExited aggregatorKey server)
      state.transPoPApi.announceAggregatorStopped agentKey server

    stoppedOtherPoP :: AgentMessageHandler
    stoppedOtherPoP state agentKey server = do
      logInfo "Remote aggregator stopped in another PoP" {agentKey, server}
      Bus.raise bus (IngestAggregatorExited (agentKeyToAggregatorKey agentKey)  server)
      sendToIntraSerfNetwork state "aggregatorStopped" (IMAggregatorState Stopped agentKey (extractAddress server))

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
announceLocalAggregatorIsAvailable :: AggregatorKey -> Effect Unit
announceLocalAggregatorIsAvailable = announceAvailableLocal aggregatorHandler <<< aggregatorKeyToAgentKey

announceLocalAggregatorStopped :: AggregatorKey -> Effect Unit
announceLocalAggregatorStopped = announceStoppedLocal aggregatorHandler <<< aggregatorKeyToAgentKey

-- -- Called by TransPoP to indicate aggregator start / stop on a node in another PoP
announceOtherPoPAggregatorIsAvailable :: AgentKey -> Server -> Effect Unit
announceOtherPoPAggregatorIsAvailable = announceAvailableOtherPoP aggregatorHandler

announceOtherPoPAggregatorStopped :: AgentKey -> Server -> Effect Unit
announceOtherPoPAggregatorStopped = announceStoppedOtherPoP aggregatorHandler

egestHandler :: AgentHandler
egestHandler
  = { name              : HandlerName "egest"
    , availableLocal    : availableLocal
    , availableThisPoP  : availableThisPoP
    , availableOtherPoP : availableOtherPoP
    , stoppedLocal      : stoppedLocal
    , stoppedThisPoP    : stoppedThisPoP
    , stoppedOtherPoP   : stoppedOtherPoP
    , gcThisPoP         : gcThisPoP
    , gcOtherPoP        : gcOtherPoP

    , clockLens         : clockLens
    , locationLens      : locationLens

    , reannounceEveryMs : _.config >>> _.reannounceAgentEveryMs >>> _.egest >>> wrap

    }
  where
    availableLocal state agentKey server = do
      sendToIntraSerfNetwork state "egestAvailable" (IMEgestState Available agentKey $ extractAddress server)

    availableThisPoP state agentKey server = do
      -- logInfo "New egest is avaiable in this PoP" {agentKey, server}
      pure unit

    availableOtherPoP state agentKey server = do
      -- Not expecting any of these
      logWarning "New egest is available in another PoP" {agentKey, server}

    stoppedLocal state agentKey server = do
      logInfo "Local egest stopped" {agentKey}
      sendToIntraSerfNetwork state "egestStopped" $ IMEgestState Stopped agentKey $ extractAddress server

    stoppedThisPoP state agentKey server = do
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


-- Egests do not have different instances for Primary and Backup, so model them all as Primary
egestKeyToAgentKey :: EgestKey -> AgentKey
egestKeyToAgentKey (EgestKey slotId) = AgentKey slotId Primary

-- Called by EgestAgent to indicate egest on this node
announceLocalEgestIsAvailable :: EgestKey -> Effect Unit
announceLocalEgestIsAvailable = announceAvailableLocal egestHandler <<< egestKeyToAgentKey

announceLocalEgestStopped :: EgestKey -> Effect Unit
announceLocalEgestStopped = announceStoppedLocal egestHandler <<< egestKeyToAgentKey

relayHandler :: AgentHandler
relayHandler
  = { name              : HandlerName "relay"
    , availableLocal    : availableLocal
    , availableThisPoP  : availableThisPoP
    , availableOtherPoP : availableOtherPoP
    , stoppedLocal      : stoppedLocal
    , stoppedThisPoP    : stoppedThisPoP
    , stoppedOtherPoP   : stoppedOtherPoP
    , gcThisPoP         : gcThisPoP
    , gcOtherPoP        : gcOtherPoP

    , clockLens         : clockLens
    , locationLens      : locationLens

    , reannounceEveryMs : _.config >>> _.reannounceAgentEveryMs >>> _.relay >>> wrap
    }
  where
    availableLocal state agentKey server = do
      sendToIntraSerfNetwork state "relayAvailable" (IMRelayState Available agentKey $ extractAddress server)

    availableThisPoP state agentKey server = do
      pure unit

    availableOtherPoP state agentKey server = do
      -- Not expecting any of these
      logWarning "New relay is available in another PoP" {agentKey, server}

    stoppedLocal state agentKey server = do
      logInfo "Local relay stopped" {agentKey}
      sendToIntraSerfNetwork state "relayStopped" $ IMRelayState Stopped agentKey $ extractAddress server

    stoppedThisPoP state agentKey server = do
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
announceLocalRelayIsAvailable (RelayKey slotId streamRole) = do
  announceAvailableLocal relayHandler (AgentKey slotId streamRole)

announceLocalRelayStopped :: RelayKey -> Effect Unit
announceLocalRelayStopped (RelayKey slotId streamRole) = do
  announceStoppedLocal relayHandler (AgentKey slotId streamRole)

-- Builds public API for events on this server
announceAvailableLocal :: AgentHandler -> AgentKey -> Effect Unit
announceAvailableLocal handler@{locationLens} agentKey =
  Gen.doCast serverName
    \state@{thisServer} -> do
      --logInfo ("New " <> unwrap handler.name <> " is available on this node") {agentKey}
      doAnnounceAvailableLocal handler agentKey state
      pure $ Gen.CastNoReply $ updateAgentLocation recordLocalAgent locationLens agentKey thisServer state

doAnnounceAvailableLocal :: AgentHandler -> AgentKey -> State -> Effect Unit
doAnnounceAvailableLocal handler@{locationLens} agentKey state@{thisServer, agentLocations} = do
  handler.availableLocal state agentKey thisServer
  void $ Timer.sendAfter serverName (unwrap $ handler.reannounceEveryMs state) $ ReAnnounce agentKey handler.name

announceStoppedLocal :: AgentHandler -> AgentKey -> Effect Unit
announceStoppedLocal handler@{locationLens} agentKey = do
  Gen.doCast serverName
    \state@{ thisServer } -> do
      logInfo (unwrap handler.name <> " stopped on this node") {agentKey}
      handler.stoppedLocal state agentKey thisServer
      pure $ Gen.CastNoReply $ updateAgentLocation removeLocalAgent locationLens agentKey thisServer state



-- Builds public API for message from other PoP
announceAvailableOtherPoP :: AgentHandler -> AgentKey -> Server -> Effect Unit
announceAvailableOtherPoP handler@{locationLens} agentKey msgServer =
  Gen.doCast serverName
    \state@{agentLocations, thisServer} -> do
      let
        locations = locationLens.get agentLocations
        _ = spy "announceAvailableOtherPoP" {name: handler.name, agentKey, msgServer}
      timeout <- messageTimeout handler state
      logIfNewAgent handler locations thisServer agentKey msgServer
      handler.availableOtherPoP state agentKey msgServer
      pure $ Gen.CastNoReply $ updateAgentLocation (recordRemoteAgent timeout) locationLens agentKey msgServer state

announceStoppedOtherPoP :: AgentHandler -> AgentKey -> Server -> Effect Unit
announceStoppedOtherPoP handler@{locationLens} agentKey server =
  Gen.doCast serverName
    \state -> do
      handler.stoppedOtherPoP state agentKey server
      pure $ Gen.CastNoReply $ updateAgentLocation removeRemoteAgent locationLens agentKey server state

-- Called by TransPoP to indicate that it is acting as this PoP's leader
announceTransPoPLeader :: Effect Unit
announceTransPoPLeader =
  Gen.doCast serverName
    \state@{ thisServer, transPoPApi:{handleRemoteLeaderAnnouncement: transPoP_announceTransPoPLeader} } ->
    do
      transPoP_announceTransPoPLeader thisServer
      sendToIntraSerfNetwork state "transPoPLeader" (IMTransPoPLeader $ extractAddress thisServer)
      pure $ Gen.CastNoReply state{currentTransPoPLeader = Just thisServer}


updateAgentLocation :: (AgentKey -> Server -> Locations -> Locations) -> AgentLocationLens -> AgentKey -> Server -> State -> State
updateAgentLocation action lens agentKey server state@{agentLocations} =
  let
    locations = lens.get agentLocations
    newLocations = action agentKey server locations
    newAgentLocations = lens.set newLocations agentLocations
  in
    state { agentLocations = newAgentLocations }


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
init { config
     , transPoPApi
     } = do

  logInfo "Intra-PoP Agent Starting" {config: config}
  healthConfig <- Config.healthConfig
  Gen.registerExternalMapping serverName (\m -> IntraPoPSerfMsg <$> (Serf.messageMapper m))

  let
    garbageCollectVMInterval = config.vmLivenessIntervalMs / 2
    garbageollectAgentInterval = ( hfoldl (min :: Int -> Int -> Int) config.reannounceAgentEveryMs.aggregator
                                     config.reannounceAgentEveryMs
                                 ) / 2

  void $ Timer.sendAfter serverName 0 JoinAll
  void $ Timer.sendEvery serverName config.rejoinEveryMs JoinAll
  void $ Timer.sendEvery serverName config.vmLivenessIntervalMs VMLiveness
  void $ Timer.sendEvery serverName garbageCollectVMInterval GarbageCollectVM
  void $ Timer.sendEvery serverName garbageollectAgentInterval GarbageCollectAgents
  rpcBindIp <- Env.privateInterfaceIp
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
      Erl.sleep (wrap 100) -- Just so we don't spin like crazy...
      unsafeCrashWith ("could_not_connect_stream")
    Right _ ->
      pure unit

  serversInPoP <- PoPDefinition.serversInThisPoPByAddress
  thisServerRef <- makeRef

  let
    busy = wrap 100.0 :: Load
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

    , testDropAgentMessages: false

    , thisServerRef
    , serverRefs: EMap.empty

    , thisServer: thisServer
    , load: wrap 0.0
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
                    }
    }

handleInfo :: Msg -> State -> Effect (CastResult State)
handleInfo msg state = case msg of

  IntraPoPSerfMsg imsg ->
    CastNoReply <$> handleIntraPoPSerfMsg imsg state

  JoinAll -> do
    joinAllSerf state
    pure $ CastNoReply state

  GarbageCollectVM ->
    CastNoReply <$> garbageCollectVM state

  GarbageCollectAgents ->
    CastNoReply <$> garbageCollectAgents state

  VMLiveness -> do
    sendToIntraSerfNetwork state "vmLiveness" (IMVMLiveness (extractAddress state.thisServer) state.thisServerRef )
    pure $ CastNoReply state

  ReAnnounce agentKey handlerName -> do
    CastNoReply <$> maybeReannounce
    where
      maybeReannounce :: Effect State
      maybeReannounce = do
        let
          handler = handlerFor handlerName
          -- Do we still know about the asset? If we do, rebroadcast its existence and set up another timer for next time
          locations = handler.locationLens.get state.agentLocations
        if mapSetMember agentKey state.thisServer locations.byAgentKey
        then do
          doAnnounceAvailableLocal handler agentKey state
          pure state
        else
          pure state

      handlerFor :: HandlerName -> AgentHandler
      handlerFor (HandlerName name) = unsafePartial $
        case name of
          "aggregator" -> aggregatorHandler
          "egest"      -> egestHandler
          "relay"      -> relayHandler




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
      Erl.sleep (wrap 100) -- Just so we don't spin like crazy...
      -- TODO send a "stepping down message?" - except without Serf I guess we can't
      unsafeCrashWith ("lost_serf_connection")

    Serf.UserEvent name ltime coalesce intraMessage -> do
      case intraMessage of
        IMAggregatorState eventType agentKey msgOrigin -> do
          handleAgentMessage ltime eventType agentKey msgOrigin state aggregatorHandler

        IMEgestState eventType agentKey msgOrigin -> do
          handleAgentMessage ltime eventType agentKey msgOrigin state egestHandler

        IMRelayState eventType agentKey msgOrigin -> do
          handleAgentMessage ltime eventType agentKey msgOrigin state relayHandler

        IMServerLoad msgOrigin load -> do
          handleServerMessage ltime msgOrigin state $ loadHandler load

        IMTransPoPLeader msgOrigin -> do
          handleServerMessage ltime msgOrigin state popLeaderHandler

        IMVMLiveness msgOrigin ref -> do
          handleServerMessage ltime msgOrigin state $ vmLivenessHandler ref


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
      mLocation <- PoPDefinition.whereIsServer msgServerAddress
      case mLocation of
        Nothing -> do
          logWarning "message from unknown server" {msgServerAddress, msgType: serverMessageHandler.name}
          pure state
        Just msgLocation -> do
          let
            msgServer = toServer msgServerAddress msgLocation
            newServerClock = Map.insert msgServerAddress msgLTime serverClock
            newServerClocks = clockLens.set newServerClock state.serverClocks
          handleMessage  msgServer state{ serverClocks = newServerClocks}


handleAgentMessage :: LamportClock -> EventType -> AgentKey -> ServerAddress -> State -> AgentHandler -> Effect State
handleAgentMessage msgLTime eventType agentKey msgServerAddress
                  state@{thisServer, agentClocks, agentLocations}
                  agentMessageHandler@{clockLens, locationLens} = do
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
      mLocation <- PoPDefinition.whereIsServer msgServerAddress
      case mLocation of
        Nothing -> do
          logWarning "message from unknown server" {msgServerAddress, msgType: agentMessageHandler.name}
          pure state
        Just msgLocation -> do
          let
            fromThisPoP =  extractPoP msgLocation == extractPoP state.thisServer
            msgServer = toServer msgServerAddress msgLocation
            newAgentClock = Map.insert (Tuple msgServerAddress agentKey) msgLTime agentClock
            newAgentClocks = clockLens.set newAgentClock state.agentClocks
            locations = locationLens.get agentLocations

          case eventType of
            Available -> do
              timeout <- messageTimeout agentMessageHandler state
              logIfNewAgent agentMessageHandler locations thisServer agentKey msgServer
              let
                newLocations = recordRemoteAgent timeout agentKey msgServer locations
              if fromThisPoP
              then agentMessageHandler.availableThisPoP state agentKey msgServer
              else pure unit
              pure $ state { agentClocks = newAgentClocks
                           , agentLocations = locationLens.set newLocations agentLocations
                           }

            Stopped -> do
              let
                newLocations = removeRemoteAgent agentKey msgServer locations
              if fromThisPoP
              then agentMessageHandler.stoppedThisPoP state agentKey msgServer
              else pure unit
              pure $ state { agentClocks = newAgentClocks
                           , agentLocations = locationLens.set newLocations agentLocations
                           }


messageTimeout :: AgentHandler -> State -> Effect Milliseconds
messageTimeout agentMessageHandler state = do
  now <- Erl.systemTimeMs
  pure $ now + ((agentMessageHandler.reannounceEveryMs state) * (wrap state.config.missCountBeforeExpiry))

--------------------------------------------------------------------------------
-- Internal functions
--------------------------------------------------------------------------------
serverName :: ServerName State Msg
serverName = Names.intraPoPName

logIfNewAgent :: AgentHandler -> Locations -> Server -> AgentKey -> Server -> Effect Unit
logIfNewAgent handler locations thisServer agentKey agentServer =
  if Map.member (Tuple agentKey agentServer) locations.remoteTimeouts
  then pure unit
  else do
    let
      scope = if extractPoP thisServer == extractPoP agentServer
              then "in this PoP"
              else "in another PoP"
    logInfo ("New " <> unwrap handler.name <> " is available " <> scope) {agentKey, agentServer}

sendToIntraSerfNetwork :: State -> String -> IntraMessage -> Effect Unit
sendToIntraSerfNetwork state name msg = do
  result <- Serf.event state.serfRpcAddress name msg false
  maybeLogError "Intra-PoP serf event failed" result {name, msg}


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

garbageCollectVM :: State -> Effect State
garbageCollectVM state@{ config
                       , serverRefs
                       } =
  do
    now <- Erl.systemTimeMs
    let
      expireThreshold = wrap $ config.missCountBeforeExpiry * config.vmLivenessIntervalMs
      threshold = now - expireThreshold
      Tuple newServerRefs garbageRefs = EMap.garbageCollect2 threshold serverRefs
      garbageServers = fst <$> garbageRefs
    foldM garbageCollectServer state{serverRefs = newServerRefs} garbageServers


garbageCollectAgents :: State -> Effect State
garbageCollectAgents state = do
  now <- Erl.systemTimeMs
  foldHandlers (gcAgent now) state

gcAgent :: Milliseconds -> AgentHandler -> State -> Effect State
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


foldHandlers :: forall acc. (AgentHandler -> acc -> Effect acc) -> acc -> Effect acc
foldHandlers perHandlerFun =
  perHandlerFun relayHandler >=> perHandlerFun aggregatorHandler >=> perHandlerFun egestHandler

garbageCollectServer :: State -> Server -> Effect State
garbageCollectServer state deadServer = do
  -- TODO - add a server decomissioning message to do this cleanly
  logWarning "server liveness timeout" {server: deadServer}
  foldHandlers (gcServer deadServer) state

gcServer :: Server -> AgentHandler -> State -> Effect State
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
      pure $ mapSetDelete agentKey deadServer acc



recordLocalAgent :: AgentKey -> Server -> Locations -> Locations
recordLocalAgent agentKey thisServer locations =
  locations { byAgentKey = mapSetInsert agentKey thisServer locations.byAgentKey
            }

removeLocalAgent :: AgentKey -> Server -> Locations -> Locations
removeLocalAgent agentKey thisServer locations =
  locations { byAgentKey = mapSetDelete agentKey thisServer locations.byAgentKey
            }


recordRemoteAgent :: Milliseconds -> AgentKey -> Server -> Locations -> Locations
recordRemoteAgent timeout agentKey server locations =
  { byAgentKey     : mapSetInsert agentKey server locations.byAgentKey
  , byRemoteServer : mapSetInsert server agentKey locations.byRemoteServer
  , remoteTimeouts : Map.insert (Tuple agentKey server) timeout locations.remoteTimeouts
  }

removeRemoteAgent :: AgentKey -> Server -> Locations -> Locations
removeRemoteAgent agentKey server locations =
  { byAgentKey     : mapSetDelete agentKey server locations.byAgentKey
  , byRemoteServer : mapSetDelete server agentKey locations.byRemoteServer
  , remoteTimeouts : Map.delete(Tuple agentKey server) locations.remoteTimeouts
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

mapSetMember :: forall k v. Ord k => Ord v => k -> v -> Map k (Set v) -> Boolean
mapSetMember k v ms = fromMaybe false $ Set.member v <$> Map.lookup k ms


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

                  maybeLogError "Intra-PoP serf join failed" result {}
                  pure unit
              )
        traverse_ joinAsync toJoin
  where
  toMap :: forall a. List a -> Map a Unit
  toMap list = foldl (\acc item -> Map.insert item unit acc) Map.empty list

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
  logInfo msg (Record.merge metadata {error: err})
  pure unit
