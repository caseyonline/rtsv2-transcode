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
import Data.Maybe (Maybe(..))
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
foreign import startWorkflowFFI :: Int -> Effect WorkflowHandle
foreign import applyPlanFFI :: WorkflowHandle -> StreamRelayPlan -> Effect StreamRelayApplyResult
foreign import setSlotConfigurationFFI :: RelayKey -> SlotConfiguration -> Effect Unit
foreign import getSlotConfigurationFFI :: RelayKey -> Effect (Maybe SlotConfiguration)

-- -----------------------------------------------------------------------------
-- Gen Server State
-- -----------------------------------------------------------------------------
data State = State StateData

type StateData =
  { relayKey :: RelayKey
  , workflowHandle :: WorkflowHandle
  , thisServer :: Server
  , ingestAggregator :: Server

  , config :: StreamRelayConfig
  , plan :: Maybe StreamRelayPlan
  , run :: StreamRelayRunState
  }

-- -----------------------------------------------------------------------------
-- Config Data Model
-- -----------------------------------------------------------------------------
type StreamRelayConfig =
  { egestSource :: EgestSource

    -- NOTE: this is a map for consistency with downstreamRelays as per below,
    --       it doesn't actually particularly matter
  , egests :: Map ServerAddress (DeliverTo EgestServer)

    -- NOTE: we key on server address, rather than server address and port
    --       because only having one active registration per server
    --       is not only strictly correct, but also simplifies
    --       cases such as a failed deregistration followed quickly by
    --       a registration
  , downstreamRelays :: Map ServerAddress DownstreamRelay
  }

data EgestSource
  = EgestSourceIngestAggregator
  | EgestSourceUpstreamRelays (List UpstreamRelay)

type DownstreamRelay =
  { deliverTo :: DeliverTo RelayServer
  , source :: DownstreamRelaySource
  }

emptyStreamRelayConfig :: EgestSource -> StreamRelayConfig
emptyStreamRelayConfig egestSource =
  { egestSource
  , egests: Map.empty
  , downstreamRelays: Map.empty
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
type StreamRelayPlan =
  { upstreamRelaySources :: List UpstreamRelaySource
  , ingestAggregatorSource :: Maybe IngestAggregatorSource
  , egests :: List (DeliverTo ServerAddress)
  }

type UpstreamRelaySource =
  { relay :: UpstreamRelay
  , downstreamRelays :: List (DeliverTo ServerAddress)
  }

type IngestAggregatorSource =
  { downstreamRelays :: List (DeliverTo ServerAddress)
  }

-- -----------------------------------------------------------------------------
-- Plan Application Result
-- -----------------------------------------------------------------------------
type StreamRelayApplyResult =
  { ingestAggregatorReceivePort :: Maybe PortNumber
  , upstreamRelayReceivePorts :: Map UpstreamRelay PortNumber
  , egests :: List (DeliverTo ServerAddress)
  }

-- -----------------------------------------------------------------------------
-- Run State Data Model
-- -----------------------------------------------------------------------------
type StreamRelayRunState =
  { slotConfiguration :: Maybe SlotConfiguration
  , upstreamRelayStates :: Map UpstreamRelay UpstreamRelayState
  , ingestAggregatorState :: IngestAggregatorState
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


emptyRunState :: StreamRelayRunState
emptyRunState =
  { slotConfiguration: Nothing
  , upstreamRelayStates: Map.empty
  , ingestAggregatorState: IngestAggregatorStateDisabled
  }

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

data DownstreamRelaySource
  = DownstreamRelaySourceIngestAggregator
  | DownstreamRelaySourceUpstreamRelay UpstreamRelay

derive instance ordDownstreamRelaySource :: Ord DownstreamRelaySource
derive instance eqDownstreamRelaySource :: Eq DownstreamRelaySource

-- -----------------------------------------------------------------------------
-- API
-- -----------------------------------------------------------------------------
registerEgest :: RegisterEgestPayload -> Effect Unit
registerEgest payload@{slotId, streamRole, deliverTo} =
  Gen.doCall (serverName $ payloadToRelayKey payload) doRegisterEgest

  where
    doRegisterEgest :: State -> Effect (CallResult Unit State)
    doRegisterEgest (State stateData@{ config: config@{ egests } }) =
      let
        newEgests = Map.insert (extractAddress deliverTo.server) deliverTo egests
        newConfig = config{ egests = newEgests }
        newPlan = configToPlan stateData newConfig
        newStateData = stateData{ config = newConfig, plan = Just newPlan }
      in
        applyPlan newStateData

registerRelay :: RegisterRelayPayload -> Effect Unit
registerRelay payload@{slotId, streamRole, sourceRoute, deliverTo } =
  Gen.doCall (serverName $ payloadToRelayKey payload) doRegisterRelay

  where
    doRegisterRelay :: State -> Effect (CallResult Unit State)
    doRegisterRelay (State stateData@{ config: config@{ downstreamRelays } }) =
      let
        source =
          case List.uncons sourceRoute of
            Just { head, tail } ->
              DownstreamRelaySourceUpstreamRelay { next: head, rest: tail }
            Nothing ->
              DownstreamRelaySourceIngestAggregator
        downstreamRelay = { source, deliverTo }
        newDownstreamRelays = Map.insert (extractAddress deliverTo.server) downstreamRelay downstreamRelays
        newConfig = config{ downstreamRelays = newDownstreamRelays }
        newPlan = configToPlan stateData newConfig
        newStateData = stateData{ config = newConfig, plan = Just newPlan }
      in
        applyPlan newStateData

applyPlan :: StateData -> Effect (CallResult Unit State)
applyPlan stateData@{ plan: Nothing } =
  pure $ CallReply unit $ State stateData
applyPlan stateData@{ workflowHandle, plan: Just plan, run: runState } =
  do
    applyResult <- applyPlanFFI workflowHandle plan
    newRunState <- applyRunResult stateData applyResult runState

    let
      newStateData = stateData{ run = newRunState }

    pure $ CallReply unit $ State newStateData

applyRunResult :: StateData -> StreamRelayApplyResult -> StreamRelayRunState -> Effect StreamRelayRunState
applyRunResult { relayKey: relayKey@(RelayKey slotId slotRole), thisServer, ingestAggregator } applyResult runState =
  (pure runState)
    <#> mergeApplyResult applyResult
    >>= maybeTryRegisterIngestAggregator
    >>= maybeTryRegisterUpstreamRelays
    >>= tryEnsureSlotConfiguration

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

    tryEnsureSlotConfiguration runStateIn@{ slotConfiguration: Just _slotConfiguration } =
      pure runStateIn

    tryEnsureSlotConfiguration runStateIn@{ ingestAggregatorState: IngestAggregatorStateRegistered _ } =
      do
        response <- fetchIngestAggregatorSlotConfiguration ingestAggregator slotId slotRole

        case hush response of
          Just receivedSlotConfiguration ->
            do
              setSlotConfigurationFFI relayKey receivedSlotConfiguration
              pure $ runStateIn{ slotConfiguration = Just receivedSlotConfiguration }

          _ ->
            tryEnsureSlotConfigurationFromRelays runStateIn

    tryEnsureSlotConfiguration runStateIn =
      tryEnsureSlotConfigurationFromRelays runStateIn

    tryEnsureSlotConfigurationFromRelays runStateIn@{ upstreamRelayStates } =
      do
        result <- tryEnsureSlotConfigurationFromRelays' (Map.values upstreamRelayStates)

        case result of
          Just receivedSlotConfiguration ->
            do
              setSlotConfigurationFFI relayKey receivedSlotConfiguration
              pure runStateIn{ slotConfiguration = Just receivedSlotConfiguration }
          _ ->
            pure runStateIn

    tryEnsureSlotConfigurationFromRelays' upstreamRelayStates =
      case List.uncons upstreamRelayStates of
        Just { head, tail } ->
          case head of
            UpstreamRelayStateRegistered portNumber serverAddress ->
              do
                response <- fetchIngestAggregatorSlotConfiguration ingestAggregator slotId slotRole

                case hush response of
                  Just receivedSlotConfiguration ->
                    pure $ Just receivedSlotConfiguration

                  _ ->
                    tryEnsureSlotConfigurationFromRelays' tail

            _ ->
              tryEnsureSlotConfigurationFromRelays' tail

        Nothing ->
          pure Nothing

isAvailable :: RelayKey -> Effect Boolean
isAvailable relayKey = isRegistered (serverName relayKey)

status :: RelayKey -> Effect (PublicState.StreamRelay List)
status =
  exposeState mkStatus <<< serverName
  where
    mkStatus (State stateData) =
       { egestsServed : Map.keys stateData.config.egests
       , relaysServed : Map.keys stateData.config.downstreamRelays
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
    workflowHandle <- startWorkflowFFI (un SlotId slotId)

    IntraPoP.announceLocalRelayIsAvailable relayKey
    _ <- Bus.subscribe (serverName relayKey) IntraPoP.bus IntraPoPBus

    pure
      $ State { relayKey
              , workflowHandle
              , thisServer
              , ingestAggregator: aggregator

              , config: emptyStreamRelayConfig $ mkEgestSource $ egestSourceRoutes
              , plan: Nothing
              , run: emptyRunState
              }
  where
    relayKey = RelayKey slotId streamRole

handleInfo :: Msg -> State -> Effect (CastResult State)
handleInfo msg state@(State {relayKey: RelayKey ourSlotId _}) =
  case msg of
    IntraPoPBus (IngestAggregatorExited (AggregatorKey slotId streamRole) serverAddress)
     -- TODO - PRIMARY BACKUP
      | slotId == ourSlotId -> doStop state
      | otherwise -> pure $ CastNoReply state

doStop :: State -> Effect (CastResult State)
doStop state@(State {relayKey}) = do
  logInfo "Stream Relay stopping" {relayKey}
  IntraPoP.announceLocalRelayStopped relayKey
  pure $ CastStop state

-- -----------------------------------------------------------------------------
-- Config -> Plan Transformation
-- -----------------------------------------------------------------------------
configToPlan :: StateData -> StreamRelayConfig -> StreamRelayPlan
configToPlan { ingestAggregator } { egests, egestSource, downstreamRelays } =

  -- TODO: PS: this is an either/or, either we're an origin relay or not
  { ingestAggregatorSource
  , upstreamRelaySources
  , egests: plannedEgests
  }

  where
    plannedEgests = egests # Map.values <#> deliverToAddressFromDeliverToEgestServer

    ingestAggregatorSource =
      if (List.nil == plannedEgests) && (List.nil == source.downstreamRelays) then
        Nothing
      else
        Just source

      where
        source =
          { downstreamRelays: downstreamRelaysForIngestAggregatorSource
          }

    egestsForIngestAggregatorSource =
      case egestSource of
        EgestSourceUpstreamRelays _upstreamRelays ->
          List.nil

        EgestSourceIngestAggregator ->
          map deliverToAddressFromDeliverToEgestServer $ Map.values egests

    downstreamRelaysForIngestAggregatorSource =
      filterMap maybeIngestAggregatorDownstreamRelay $ Map.values downstreamRelays

      where
        maybeIngestAggregatorDownstreamRelay { source, deliverTo } =
          case source of
            DownstreamRelaySourceIngestAggregator ->
              Just $ deliverToAddressFromDeliverToRelayServer deliverTo
            DownstreamRelaySourceUpstreamRelay _upstreamRelay ->
              Nothing

    upstreamRelaySources =
      Map.values result

      where
        relaySourcesWithDownstreamRelays =
          Map.mapWithKey mkRelaySource downstreamRelaysBySource

        mkRelaySource upstreamRelay downstreamRelays =
          { relay: upstreamRelay
          , downstreamRelays
          }

        relaySourcesWithEgests =
          case Map.size egests of
            0 ->
              relaySourcesWithDownstreamRelays

            _ ->
              case egestSource of
                EgestSourceIngestAggregator ->
                  relaySourcesWithDownstreamRelays

                EgestSourceUpstreamRelays upstreamRelays ->
                  foldl addEgestsToRelays relaySourcesWithDownstreamRelays upstreamRelays

        addEgestsToRelays upstreamRelays relayToAdd =
          Map.alter (addEgestsToRelay relayToAdd) relayToAdd upstreamRelays

        addEgestsToRelay relayToAdd Nothing =
            Just { relay: relayToAdd, downstreamRelays: List.nil }

        addEgestsToRelay _relayToAdd existingRelay =
          existingRelay

        result = relaySourcesWithEgests

    downstreamRelaysBySource =
      foldl foldRelay Map.empty $ Map.values downstreamRelays

      where
        foldRelay downstreamRelaysByUpstreamRelay { source, deliverTo } =
          let
            deliverToAddress = deliverToAddressFromDeliverToRelayServer deliverTo
          in
            case source of
              DownstreamRelaySourceIngestAggregator ->
                downstreamRelaysByUpstreamRelay

              DownstreamRelaySourceUpstreamRelay upstreamRelay ->
                Map.alter alter upstreamRelay downstreamRelaysByUpstreamRelay

                where
                  alter (Just relaysForSource) =
                    Just (deliverToAddress : relaysForSource)

                  alter Nothing =
                    Just (deliverToAddress : List.nil)

    deliverToAddressFromDeliverToEgestServer { server, port } =
      { server: extractAddress server, port }

    deliverToAddressFromDeliverToRelayServer { server, port } =
      { server: extractAddress server, port }

-- -----------------------------------------------------------------------------
-- Apply Result -> Run State Transformation
-- -----------------------------------------------------------------------------
mergeApplyResult :: StreamRelayApplyResult -> StreamRelayRunState -> StreamRelayRunState
mergeApplyResult { ingestAggregatorReceivePort, upstreamRelayReceivePorts } runState@{ upstreamRelayStates, ingestAggregatorState } =
  runState{ upstreamRelayStates = withTombstonedAndUpsertedEntries
          , ingestAggregatorState = updatedIngestAggregatorState
          }

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


--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------
mkEgestSource :: PoPRoutes -> EgestSource
mkEgestSource routes =
  if routes == List.nil then
    EgestSourceIngestAggregator
  else
    EgestSourceUpstreamRelays (map mkUpstreamRelay routes)

mkUpstreamRelay :: SourceRoute -> UpstreamRelay
mkUpstreamRelay route =
  let
    { head: next, tail: rest } = Unsafe.unsafePartial $ Maybe.fromJust $ List.uncons route
  in
    { next, rest }

fetchIngestAggregatorSlotConfiguration :: Server -> SlotId -> SlotRole -> Effect (Either JsonResponseError SlotConfiguration)
fetchIngestAggregatorSlotConfiguration aggregator slotId streamRole =
  fetchSlotConfiguration $ makeUrl aggregator $ IngestAggregatorSlotConfigurationE slotId streamRole

fetchStreamRelaySlotConfiguration :: Server -> SlotId -> SlotRole -> Effect (Either JsonResponseError SlotConfiguration)
fetchStreamRelaySlotConfiguration relay slotId streamRole =
  fetchSlotConfiguration $ makeUrl relay $ RelaySlotConfigurationE slotId streamRole

fetchSlotConfiguration :: Url -> Effect (Either JsonResponseError SlotConfiguration)
fetchSlotConfiguration url =
  SpudGun.getJson url <#> SpudGun.bodyToJSON

extractServedByHeader :: SpudResponse -> Maybe ServerAddress
extractServedByHeader (SpudResponse _statusCode headers _body) =
  headers
    # find (\tuple -> ErlTuple.fst tuple == "x-servedby")
    # map  (\tuple -> ServerAddress (ErlTuple.snd tuple))

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
