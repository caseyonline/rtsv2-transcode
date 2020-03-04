-- StreamRelayInstance
--
-- Important Terms:
--   Upstream Relay
--     A relay that is nearer to the Ingest Aggregator relative to another relay
--
--   Downstream Relay
--     A relay that is further away from the Ingest Aggregator relative to another relay.
--     NOTE: this also implies that the relay is NOT an origin relay
--
--   Origin Relay
--     A relay that is in the same PoP as the Ingest Aggregator.
--
module Rtsv2.Agents.StreamRelayInstance
       ( startLink
       , isAvailable
       , status
       , registerEgest
       , registerRelay
       , slotConfiguration
       , init
       , State
       ) where

import Prelude

import Bus as Bus
import Data.Either (Either(..), hush)
import Data.Filterable (filterMap)
import Data.Foldable (find, foldl)
import Data.FoldableWithIndex (foldlWithIndex)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Maybe as Maybe
import Data.Newtype (un)
import Data.Set as Set
import Data.TraversableWithIndex (traverseWithIndex)
import Effect (Effect)
import Erl.Atom (Atom, atom)
import Erl.Data.List (List, (:))
import Erl.Data.List as List
import Erl.Data.Map (Map)
import Erl.Data.Map as Map
import Erl.Data.Tuple as ErlTuple
import Logger (Logger)
import Logger as Logger
import Partial.Unsafe as Unsafe
import Pinto (ServerName, StartLinkResult, isRegistered)
import Pinto.Gen (CallResult(..), CastResult(..))
import Pinto.Gen as Gen
import PintoHelper (exposeState)
import Rtsv2.Agents.IntraPoP (IntraPoPBusMessage(..))
import Rtsv2.Agents.IntraPoP as IntraPoP
import Rtsv2.Agents.SlotTypes (SlotConfiguration)
import Rtsv2.Agents.StreamRelayTypes (CreateRelayPayload, RegisterEgestPayload, SourceRoute, RegisterRelayPayload)
import Rtsv2.Agents.TransPoP (PoPRoutes)
import Rtsv2.Agents.TransPoP as TransPoP
import Rtsv2.Names as Names
import Rtsv2.PoPDefinition as PoPDefinition
import Shared.Agent as Agent
import Shared.Stream (AggregatorKey(..), RelayKey(..), SlotId(..), SlotRole)
import Shared.Router.Endpoint (Endpoint(..), makeUrlAddr, makeUrl)
import Shared.Types (PoPName, DeliverTo, EgestServer, RelayServer(..), Server(..), ServerAddress(..), Url, extractPoP, extractAddress)
import Shared.Types.Agent.State as PublicState
import SpudGun (SpudResponse(..), JsonResponseError, StatusCode(..))
import SpudGun as SpudGun

-- -----------------------------------------------------------------------------
-- FFI
-- -----------------------------------------------------------------------------
foreign import data WorkflowHandle :: Type

foreign import startOriginWorkflowFFI :: Int -> Effect WorkflowHandle
foreign import applyOriginPlanFFI :: OriginStreamRelayPlan -> WorkflowHandle -> Effect OriginStreamRelayApplyResult

foreign import startDownstreamWorkflowFFI :: Int -> Effect WorkflowHandle
foreign import applyDownstreamPlanFFI :: DownstreamStreamRelayPlan -> WorkflowHandle -> Effect DownstreamStreamRelayApplyResult

foreign import setSlotConfigurationFFI :: RelayKey -> SlotConfiguration -> Effect Unit
foreign import getSlotConfigurationFFI :: RelayKey -> Effect (Maybe SlotConfiguration)

-- -----------------------------------------------------------------------------
-- Gen Server State
-- -----------------------------------------------------------------------------
data State
  = StateOrigin CommonStateData OriginStreamRelayStateData
  | StateDownstream CommonStateData DownstreamStreamRelayStateData

type CommonStateData =
  { relayKey :: RelayKey
  , thisServer :: Server
  , ingestAggregator :: Server
  }

type OriginStreamRelayStateData =
  { workflowHandle :: WorkflowHandle
  , config :: OriginStreamRelayConfig
  , plan :: Maybe OriginStreamRelayPlan
  , run :: OriginStreamRelayRunState
  }

type DownstreamStreamRelayStateData =
  { workflowHandle :: WorkflowHandle
  , config :: DownstreamStreamRelayConfig
  , plan :: Maybe DownstreamStreamRelayPlan
  , run :: DownstreamStreamRelayRunState
  }

-- -----------------------------------------------------------------------------
-- Config Data Model
-- -----------------------------------------------------------------------------
--
-- NOTE: we key on server address, rather than server address and port
--       because only having one active registration per server
--       is not only strictly correct, but also simplifies
--       cases such as a failed deregistration followed quickly by
--       a registration
--
type OriginStreamRelayConfig =
  { egests :: Map ServerAddress (DeliverTo EgestServer)
  , downstreamRelays :: Map ServerAddress (DeliverTo RelayServer)
  }

type DownstreamStreamRelayConfig =
  { egestUpstreamRelays :: List UpstreamRelay
  , egests :: Map ServerAddress (DeliverTo EgestServer)
  , downstreamRelays :: Map ServerAddress DownstreamRelayWithSource
  }

type DownstreamRelayWithSource =
  { deliverTo :: DeliverTo RelayServer
  , source :: UpstreamRelay
  }

-- -----------------------------------------------------------------------------
-- Plan Data Model
--
-- WARNING:
-- Be careful about changes here, these data are sent to the FFI code!
--
-- NOTE:
-- Lists here are really sets, but we use lists to be readily readable from
-- Erlang
-- -----------------------------------------------------------------------------
type OriginStreamRelayPlan =
  { egests :: List (DeliverTo ServerAddress)
  , downstreamRelays :: List (DeliverTo ServerAddress)
  }

type DownstreamStreamRelayPlan =
  { upstreamRelaySources :: List UpstreamRelay
  , egests :: List (DeliverTo ServerAddress)
  , downstreamRelays :: List (DeliverTo ServerAddress)
  }

-- -----------------------------------------------------------------------------
-- Plan Application Result
-- -----------------------------------------------------------------------------
type OriginStreamRelayApplyResult =
  { ingestAggregatorReceivePort :: Maybe PortNumber
  }

type DownstreamStreamRelayApplyResult =
  { upstreamRelayReceivePorts :: Map UpstreamRelay PortNumber
  }

-- -----------------------------------------------------------------------------
-- Run State Data Model
-- -----------------------------------------------------------------------------
type OriginStreamRelayRunState =
  { slotConfiguration :: Maybe SlotConfiguration
  , ingestAggregatorState :: IngestAggregatorState
  }

type DownstreamStreamRelayRunState =
  { slotConfiguration :: Maybe SlotConfiguration
  , upstreamRelayStates :: Map UpstreamRelay UpstreamRelayState
  }

data UpstreamRelayState
  = UpstreamRelayStatePendingRegistration PortNumber
  | UpstreamRelayStateRegistered PortNumber ServerAddress
  | UpstreamRelayStatePendingDeregistration PortNumber ServerAddress
  | UpstreamRelayStateDeregistered PortNumber

data IngestAggregatorState
  = IngestAggregatorStateDisabled
  | IngestAggregatorStatePendingRegistration PortNumber
  | IngestAggregatorStateRegistered PortNumber

-- -----------------------------------------------------------------------------
-- Shared Data Model Types
--
-- WARNING:
-- Be careful about changes here, some of these data are sent to the FFI code!
-- -----------------------------------------------------------------------------
type PortNumber = Int
type Host = String

type UpstreamRelay =
  { next :: PoPName
  , rest :: List PoPName
  }

-- -----------------------------------------------------------------------------
-- API
-- -----------------------------------------------------------------------------
registerEgest :: RegisterEgestPayload -> Effect Unit
registerEgest payload@{slotId, streamRole, deliverTo} =
  Gen.doCall (serverName $ payloadToRelayKey payload) doRegisterEgest

  where
    doRegisterEgest :: State -> Effect (CallResult Unit State)

    doRegisterEgest (StateOrigin commonStateData originStateData@{ config: config@{ egests } }) =
      let
        newEgests = Map.insert (extractAddress deliverTo.server) deliverTo egests
        newConfig = config{ egests = newEgests }
        newPlan = originConfigToPlan commonStateData newConfig
        newOriginStateData = originStateData{ config = newConfig, plan = Just newPlan }
      in
        applyOriginPlan commonStateData newOriginStateData

    doRegisterEgest (StateDownstream commonStateData downstreamStateData@{ config: config@{ egests } }) =
      let
        newEgests = Map.insert (extractAddress deliverTo.server) deliverTo egests
        newConfig = config{ egests = newEgests }
        newPlan = downstreamConfigToPlan commonStateData newConfig
        newDownstreamStateData = downstreamStateData{ config = newConfig, plan = Just newPlan }
      in
        applyDownstreamPlan commonStateData newDownstreamStateData

registerRelay :: RegisterRelayPayload -> Effect Unit
registerRelay payload@{slotId, streamRole, sourceRoute, deliverTo } =
  Gen.doCall (serverName $ payloadToRelayKey payload) doRegisterRelay

  where
    doRegisterRelay :: State -> Effect (CallResult Unit State)

    doRegisterRelay (StateOrigin commonStateData originStateData@{ config: config@{ downstreamRelays } }) =
      -- TODO: PS: log if we've got a non-nil source when we're in origin mode
      let
        newDownstreamRelays = Map.insert (extractAddress deliverTo.server) deliverTo downstreamRelays
        newConfig = config{ downstreamRelays = newDownstreamRelays }
        newPlan = originConfigToPlan commonStateData newConfig
        newOriginStateData = originStateData{ config = newConfig, plan = Just newPlan }
      in
        applyOriginPlan commonStateData newOriginStateData

    doRegisterRelay (StateDownstream commonStateData downstreamStateData@{ config: config@{ downstreamRelays } }) =
      case List.uncons sourceRoute of
        Just { head, tail } ->
          let
            downstreamRelayWithSource =
              { source: { next: head, rest: tail }
              , deliverTo
              }
            newDownstreamRelays = Map.insert (extractAddress deliverTo.server) downstreamRelayWithSource downstreamRelays
            newConfig = config{ downstreamRelays = newDownstreamRelays }
            newPlan = downstreamConfigToPlan commonStateData newConfig
            newDownstreamStateData = downstreamStateData{ config = newConfig, plan = Just newPlan }
          in
            applyDownstreamPlan commonStateData newDownstreamStateData

        Nothing ->
          -- TODO: PS: log if we've got a nil source when we're in downstream mode
          pure $ CallReply unit $ StateDownstream commonStateData downstreamStateData

applyOriginPlan :: CommonStateData -> OriginStreamRelayStateData -> Effect (CallResult Unit State)

applyOriginPlan commonStateData originStateData@{ plan: Nothing } =
  pure $ CallReply unit $ StateOrigin commonStateData originStateData

applyOriginPlan commonStateData originStateData@{ plan: Just plan, run: runState, workflowHandle } =
  do
    applyResult <- applyOriginPlanFFI plan workflowHandle

    newRunState <- applyOriginRunResult commonStateData applyResult runState

    pure $ CallReply unit $ StateOrigin commonStateData $ originStateData{ run = newRunState }

applyDownstreamPlan :: CommonStateData -> DownstreamStreamRelayStateData -> Effect (CallResult Unit State)

applyDownstreamPlan commonStateData downstreamStateData@{ plan: Nothing } =
  pure $ CallReply unit $ StateDownstream commonStateData downstreamStateData

applyDownstreamPlan commonStateData downstreamStateData@{ plan: Just plan, run: runState, workflowHandle } =
  do
    applyResult <- applyDownstreamPlanFFI plan workflowHandle

    newRunState <- applyDownstreamRunResult commonStateData applyResult runState

    pure $ CallReply unit $ StateDownstream commonStateData $ downstreamStateData{ run = newRunState }

applyOriginRunResult :: CommonStateData -> OriginStreamRelayApplyResult -> OriginStreamRelayRunState -> Effect OriginStreamRelayRunState
applyOriginRunResult commonStateData@{ relayKey: relayKey@(RelayKey slotId slotRole), thisServer, ingestAggregator } applyResult runState =
  (pure runState)
    <#> mergeOriginApplyResult applyResult
    >>= maybeTryRegisterIngestAggregator
    >>= tryEnsureOriginSlotConfiguration commonStateData

  where
    maybeTryRegisterIngestAggregator runStateIn@{ ingestAggregatorState: IngestAggregatorStateDisabled } = pure runStateIn
    maybeTryRegisterIngestAggregator runStateIn@{ ingestAggregatorState: IngestAggregatorStateRegistered _ } = pure runStateIn
    maybeTryRegisterIngestAggregator runStateIn@{ ingestAggregatorState: IngestAggregatorStatePendingRegistration portNumber } =
      let
        registerURL = makeUrl ingestAggregator IngestAggregatorRegisterRelayE

        deliverTo =
          { server: (thisServer # un Server # Relay)
          , port: portNumber
          }

        -- Should this be a different type? sourceRoute is redundant when registered with an IA
        registerPayload :: RegisterRelayPayload
        registerPayload = {slotId, streamRole: slotRole, deliverTo, sourceRoute: List.nil}
      in
        do
           response <- SpudGun.postJson registerURL registerPayload

           logInfo "Attempted registration with ingest aggregator." { registerPayload, response }

           case response of
             Right (SpudResponse (StatusCode statusCode) _headers _body) | statusCode >= 200 && statusCode < 300 ->
               pure runStateIn{ ingestAggregatorState = IngestAggregatorStateRegistered portNumber }

             _other ->
               pure runStateIn

applyDownstreamRunResult :: CommonStateData -> DownstreamStreamRelayApplyResult -> DownstreamStreamRelayRunState -> Effect DownstreamStreamRelayRunState
applyDownstreamRunResult commonStateData@{ relayKey: relayKey@(RelayKey slotId slotRole), thisServer, ingestAggregator } applyResult runState =
  (pure runState)
    <#> mergeDownstreamApplyResult applyResult
    >>= maybeTryRegisterUpstreamRelays
    >>= tryEnsureDownstreamSlotConfiguration commonStateData

  where
    maybeTryRegisterUpstreamRelays runStateIn@{ upstreamRelayStates } =
      do
        newUpstreamRelayStates <- traverseWithIndex maybeTryRegisterUpstreamRelay upstreamRelayStates
        pure $ runStateIn{ upstreamRelayStates = newUpstreamRelayStates }

    maybeTryRegisterUpstreamRelay :: UpstreamRelay -> UpstreamRelayState -> Effect UpstreamRelayState
    maybeTryRegisterUpstreamRelay upstreamRelay upstreamRelayStateIn =
      case upstreamRelayStateIn of
        UpstreamRelayStatePendingRegistration portNumber ->
          tryRegisterUpstreamRelay upstreamRelay portNumber

        UpstreamRelayStatePendingDeregistration portNumber serverAddress ->
          -- TODO: PS: deregistrations
          pure upstreamRelayStateIn

        _alreadyRegisteredOrDeregistered ->
          pure upstreamRelayStateIn

    tryRegisterUpstreamRelay { next, rest } portNumber =
      do
        maybeRelayAddress <- ensureRelayInPoP next

        _ <- (logInfo "Ensured relay running in pop" { pop: next, maybeRelayAddress })

        case maybeRelayAddress of
          Nothing ->
            pure $ UpstreamRelayStatePendingRegistration portNumber

          Just relayAddress ->
            do
              registerResult <- registerWithSpecificRelay portNumber relayAddress rest

              _ <- (logInfo "Attempted registration with relay" { registerResult, relayAddress, portNumber })

              if registerResult then
                pure $ UpstreamRelayStateRegistered portNumber relayAddress
              else
                pure $ UpstreamRelayStatePendingRegistration portNumber

    ensureRelayInPoP pop =
      do
        maybeRandomServerInPoP <- PoPDefinition.getRandomServerInPoP pop

        case maybeRandomServerInPoP of
          Nothing ->
            pure Nothing

          Just randomServerInPoP ->
            let
              payload = { slotId, streamRole: slotRole, aggregator: ingestAggregator } :: CreateRelayPayload
              url = makeUrlAddr randomServerInPoP RelayEnsureStartedE
            in
              do
                eitherResponse <- SpudGun.postJsonFollow url payload

                case eitherResponse of
                  Right response@(SpudResponse (StatusCode statusCode) _headers _body) | statusCode >= 200 && statusCode < 300 ->
                    pure $ extractServedByHeader response

                  _other ->
                    pure $ Nothing

    registerWithSpecificRelay portNumber chosenRelay remainingRoute =
      let
        deliverTo =
          { server: (thisServer # un Server # Relay)
          , port: portNumber
          }

        payload =
          { slotId
          , streamRole: slotRole
          , deliverTo
          , sourceRoute: remainingRoute
          }

        url = makeUrlAddr chosenRelay RelayRegisterRelayE
      in
        do

        -- Don't use follow here as we should be talking to the correct server directly
        eitherResponse <- SpudGun.postJson url payload

        case eitherResponse of
          Right response@(SpudResponse (StatusCode statusCode) _headers _body) | statusCode >= 200 && statusCode < 300 ->
            pure true

          _other ->
            pure false

tryEnsureOriginSlotConfiguration :: CommonStateData -> OriginStreamRelayRunState -> Effect OriginStreamRelayRunState

tryEnsureOriginSlotConfiguration _commonStateData runStateIn@{ slotConfiguration: Just _ } =
  pure runStateIn

tryEnsureOriginSlotConfiguration { relayKey: relayKey@(RelayKey slotId slotRole), ingestAggregator } runStateIn =
  do
    response <- fetchIngestAggregatorSlotConfiguration ingestAggregator slotId slotRole

    case hush response of
      Nothing ->
        pure runStateIn

      Just receivedSlotConfiguration ->
        do
          setSlotConfigurationFFI relayKey receivedSlotConfiguration
          pure runStateIn{ slotConfiguration = Just receivedSlotConfiguration }

tryEnsureDownstreamSlotConfiguration :: CommonStateData -> DownstreamStreamRelayRunState -> Effect DownstreamStreamRelayRunState

tryEnsureDownstreamSlotConfiguration _commonStateData runStateIn@{ slotConfiguration: Just _ } =
  pure runStateIn

tryEnsureDownstreamSlotConfiguration { relayKey: relayKey@(RelayKey slotId slotRole) } runStateIn@{ upstreamRelayStates } =
  do
    result <- tryEnsureSlotConfigurationFromRelays' (Map.values upstreamRelayStates)

    case result of
      Nothing ->
        pure runStateIn

      Just receivedSlotConfiguration ->
        do
          setSlotConfigurationFFI relayKey receivedSlotConfiguration
          pure runStateIn{ slotConfiguration = Just receivedSlotConfiguration }

  where
    tryEnsureSlotConfigurationFromRelays' upstreamRelayStates =
      case List.uncons upstreamRelayStates of
        Nothing ->
          pure Nothing

        Just { head, tail } ->
          case head of
            UpstreamRelayStateRegistered portNumber serverAddress ->
              do
                response <- fetchStreamRelaySlotConfiguration serverAddress slotId slotRole

                case hush response of
                  Just receivedSlotConfiguration ->
                    pure $ Just receivedSlotConfiguration

                  _ ->
                    tryEnsureSlotConfigurationFromRelays' tail

            relayInOtherState ->
              tryEnsureSlotConfigurationFromRelays' tail

isAvailable :: RelayKey -> Effect Boolean
isAvailable relayKey = isRegistered (serverName relayKey)

status :: RelayKey -> Effect (PublicState.StreamRelay List)
status =
  exposeState mkStatus <<< serverName
  where
    mkStatus (StateOrigin _commonStateData originStateData) =
       { egestsServed : Map.keys originStateData.config.egests
       , relaysServed : Map.keys originStateData.config.downstreamRelays
       }
    mkStatus (StateDownstream _commonStateData downstreamStateData) =
       { egestsServed : Map.keys downstreamStateData.config.egests
       , relaysServed : Map.keys downstreamStateData.config.downstreamRelays
       }

slotConfiguration :: RelayKey -> Effect (Maybe SlotConfiguration)
slotConfiguration relayKey =
  getSlotConfigurationFFI relayKey

-- -----------------------------------------------------------------------------
-- Gen Server Implementation
-- -----------------------------------------------------------------------------
data Msg = IntraPoPBus IntraPoP.IntraPoPBusMessage

payloadToRelayKey :: forall r. { slotId :: SlotId, streamRole :: SlotRole | r } -> RelayKey
payloadToRelayKey payload = RelayKey payload.slotId payload.streamRole

serverName :: RelayKey -> ServerName State Msg
serverName = Names.streamRelayInstanceName

startLink :: CreateRelayPayload -> Effect StartLinkResult
startLink payload =
  let
    relayKey = payloadToRelayKey payload
  in
    Gen.startLink (serverName relayKey) (init payload) handleInfo

init :: CreateRelayPayload -> Effect State
init payload@{slotId, streamRole, aggregator} =
  do
    thisServer <- PoPDefinition.getThisServer
    egestSourceRoutes <- TransPoP.routesTo (extractPoP aggregator)

    IntraPoP.announceLocalRelayIsAvailable relayKey
    _ <- Bus.subscribe (serverName relayKey) IntraPoP.bus IntraPoPBus

    let
      commonStateData =
        { relayKey
        , thisServer
        , ingestAggregator: aggregator
        }

    if egestSourceRoutes == List.nil then
      do
        workflowHandle <- startOriginWorkflowFFI (un SlotId slotId)

        let
          originStateData =
            { workflowHandle
            , config: { egests: Map.empty, downstreamRelays: Map.empty }
            , plan: Nothing
            , run: { slotConfiguration: Nothing, ingestAggregatorState: IngestAggregatorStateDisabled }
            }

        pure $ StateOrigin commonStateData originStateData
    else
      do
        workflowHandle <- startDownstreamWorkflowFFI (un SlotId slotId)

        let
          egestUpstreamRelays = map mkUpstreamRelay egestSourceRoutes

          downstreamStateData =
            { workflowHandle
            , config: { egestUpstreamRelays, egests: Map.empty, downstreamRelays: Map.empty }
            , plan: Nothing
            , run: { slotConfiguration: Nothing, upstreamRelayStates: Map.empty }
            }

        pure $ StateDownstream commonStateData downstreamStateData

  where
    relayKey = RelayKey slotId streamRole

handleInfo :: Msg -> State -> Effect (CastResult State)
handleInfo msg state =
  case msg of
    IntraPoPBus (IngestAggregatorExited (AggregatorKey slotId streamRole) serverAddress)
     -- TODO - PRIMARY BACKUP
      | slotId == ourSlotId -> doStop state
      | otherwise -> pure $ CastNoReply state
  where
    (RelayKey ourSlotId _) = relayKeyFromState state

doStop :: State -> Effect (CastResult State)
doStop state =
  do
    logInfo "Stream Relay stopping" {relayKey}
    IntraPoP.announceLocalRelayStopped relayKey
    pure $ CastStop state
  where
    relayKey = relayKeyFromState state

-- -----------------------------------------------------------------------------
-- Config -> Plan Transformation
-- -----------------------------------------------------------------------------
originConfigToPlan :: CommonStateData -> OriginStreamRelayConfig -> OriginStreamRelayPlan
originConfigToPlan { ingestAggregator } { egests, downstreamRelays } =
  { egests: plannedEgests
  , downstreamRelays: plannedDownstreamRelays
  }
  where
    plannedEgests = egests # Map.values # map deliverToAddressFromDeliverToEgestServer
    plannedDownstreamRelays = downstreamRelays # Map.values # map deliverToAddressFromDeliverToRelayServer

downstreamConfigToPlan :: CommonStateData -> DownstreamStreamRelayConfig -> DownstreamStreamRelayPlan
downstreamConfigToPlan { ingestAggregator } config@{ egestUpstreamRelays } =
  { egests: plannedEgests
  , downstreamRelays: plannedDownstreamRelays
  , upstreamRelaySources
  }
  where
    egestList = config.egests # Map.values

    plannedEgests = egestList <#> deliverToAddressFromDeliverToEgestServer

    plannedDownstreamRelays = downstreamRelayList # map (_.deliverTo) # map deliverToAddressFromDeliverToRelayServer

    downstreamRelayList = config.downstreamRelays # Map.values

    upstreamRelaySources =
      Map.empty
        # ensureRelaySourcesForEgests egestList egestUpstreamRelays
        # ensureRelaySourcesForDownstreamRelays downstreamRelayList
        # Map.values

    ensureRelaySourcesForEgests egests egestUpstreamRelays upstreamRelaySources =
      if egests == List.nil then
        upstreamRelaySources
      else
        foldl (\z relay -> ensureRelaySource relay z) upstreamRelaySources egestUpstreamRelays

    ensureRelaySourcesForDownstreamRelays downstreamRelays upstreamRelaySources =
      downstreamRelays
        # map (\relay -> { upstream: relay.source, downstream: deliverToAddressFromDeliverToRelayServer relay.deliverTo })
        # foldl (\z relay -> ensureRelaySource relay.upstream z) upstreamRelaySources

    ensureRelaySource upstreamRelay upstreamRelaySources =
      Map.alter (alterRelaySource upstreamRelay) upstreamRelay upstreamRelaySources

    alterRelaySource relay Nothing =
      Just relay

    alterRelaySource relay (Just existingRelaySource) =
      Just existingRelaySource

-- -----------------------------------------------------------------------------
-- Apply Result -> Run State Transformation
-- -----------------------------------------------------------------------------
mergeOriginApplyResult :: OriginStreamRelayApplyResult -> OriginStreamRelayRunState -> OriginStreamRelayRunState
mergeOriginApplyResult { ingestAggregatorReceivePort } runState@{ ingestAggregatorState } =
  runState{ ingestAggregatorState = updatedIngestAggregatorState }

  where
    updatedIngestAggregatorState =
      case ingestAggregatorReceivePort of
        Nothing ->
          -- TODO: PS: deregistration?
          ingestAggregatorState

        Just receivePort ->
          case ingestAggregatorState of
            IngestAggregatorStateDisabled ->
              IngestAggregatorStatePendingRegistration receivePort

            IngestAggregatorStatePendingRegistration _portNumber ->
              -- TODO: PS: validate the port number matches?
              ingestAggregatorState

            IngestAggregatorStateRegistered _portNumber ->
              -- TODO: PS: validate the port number matches?
              ingestAggregatorState

mergeDownstreamApplyResult :: DownstreamStreamRelayApplyResult -> DownstreamStreamRelayRunState -> DownstreamStreamRelayRunState
mergeDownstreamApplyResult { upstreamRelayReceivePorts } runState@{ upstreamRelayStates } =
  runState{ upstreamRelayStates = withTombstonedAndUpsertedEntries }

  where
    withTombstonedEntries =
      Map.mapWithKey maybeTombstoneEntry upstreamRelayStates

    maybeTombstoneEntry relay existingRelayState =
      if Map.member relay upstreamRelayReceivePorts then
        existingRelayState
      else
        case existingRelayState of
            UpstreamRelayStatePendingRegistration portNumber ->
              -- It was never regsitered, we can skip straight to deregistered
              UpstreamRelayStateDeregistered portNumber

            UpstreamRelayStateRegistered portNumber serverAddress ->
              -- It was previously registered, unregister it
              UpstreamRelayStatePendingDeregistration portNumber serverAddress

            UpstreamRelayStatePendingDeregistration _portNumber _serverAddress ->
              existingRelayState

            UpstreamRelayStateDeregistered existingPortNumber ->
              existingRelayState

    withTombstonedAndUpsertedEntries =
      foldlWithIndex upsertEntry withTombstonedEntries upstreamRelayReceivePorts

    upsertEntry relay relayStatesIn portNumber =
      case Map.lookup relay relayStatesIn of
        Nothing ->
          Map.insert relay (UpstreamRelayStatePendingRegistration portNumber) relayStatesIn

        Just existingRelayState ->

          case existingRelayState of
            UpstreamRelayStatePendingRegistration _portNumber ->
              -- TODO: PS: validate the port number matches?
              relayStatesIn

            UpstreamRelayStateRegistered existingPortNumber existingServerAddress ->
              -- TODO: PS: validate the port number matches?
              relayStatesIn

            UpstreamRelayStatePendingDeregistration _portNumber _serverAddress ->

              -- We never successfully deregistered, that doesn't matter though, we can just
              -- reregister and that will obsolete the previous registration anyway
              Map.insert relay (UpstreamRelayStatePendingRegistration portNumber) relayStatesIn

            UpstreamRelayStateDeregistered existingPortNumber ->

              -- This is the same as the "Nothing" case, it's just that the entry hasn't been
              -- pruned yet
              Map.insert relay (UpstreamRelayStatePendingRegistration portNumber) relayStatesIn

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------
mkUpstreamRelay :: SourceRoute -> UpstreamRelay
mkUpstreamRelay route =
  let
    { head: next, tail: rest } = Unsafe.unsafePartial $ Maybe.fromJust $ List.uncons route
  in
    { next, rest }

fetchIngestAggregatorSlotConfiguration :: Server -> SlotId -> SlotRole -> Effect (Either JsonResponseError SlotConfiguration)
fetchIngestAggregatorSlotConfiguration aggregator slotId streamRole =
  fetchSlotConfiguration $ makeUrl aggregator $ IngestAggregatorSlotConfigurationE slotId streamRole

fetchStreamRelaySlotConfiguration :: ServerAddress -> SlotId -> SlotRole -> Effect (Either JsonResponseError SlotConfiguration)
fetchStreamRelaySlotConfiguration relay slotId streamRole =
  fetchSlotConfiguration $ makeUrlAddr relay $ RelaySlotConfigurationE slotId streamRole

fetchSlotConfiguration :: Url -> Effect (Either JsonResponseError SlotConfiguration)
fetchSlotConfiguration url =
  SpudGun.getJson url <#> SpudGun.bodyToJSON

extractServedByHeader :: SpudResponse -> Maybe ServerAddress
extractServedByHeader (SpudResponse _statusCode headers _body) =
  headers
    # find (\tuple -> ErlTuple.fst tuple == "x-servedby")
    # map  (\tuple -> ServerAddress (ErlTuple.snd tuple))

deliverToAddressFromDeliverToEgestServer :: DeliverTo EgestServer -> DeliverTo ServerAddress
deliverToAddressFromDeliverToEgestServer { server, port } =
  { server: extractAddress server, port }

deliverToAddressFromDeliverToRelayServer :: DeliverTo RelayServer -> DeliverTo ServerAddress
deliverToAddressFromDeliverToRelayServer { server, port } =
  { server: extractAddress server, port }

relayKeyFromState :: State -> RelayKey
relayKeyFromState (StateOrigin { relayKey } _) = relayKey
relayKeyFromState (StateDownstream { relayKey } _) = relayKey

--------------------------------------------------------------------------------
-- Log Utilities
--------------------------------------------------------------------------------
domains :: List Atom
domains = atom <$> (show Agent.StreamRelay :  "Instance" : List.nil)

logInfo :: forall a. Logger a
logInfo = domainLog Logger.info

logWarning :: forall a. Logger a
logWarning = domainLog Logger.warning

logError :: forall a. Logger a
logError = domainLog Logger.error

domainLog :: forall a. Logger {domain :: List Atom, misc :: a} -> Logger a
domainLog = Logger.doLog domains
