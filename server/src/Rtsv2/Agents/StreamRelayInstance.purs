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
       (
         startLink
       , dataObjectSendMessage
       , dataObjectUpdateSendMessage
       , domain
       , forceDrain
       , init
       , isInstanceAvailable
       , payloadToRelayKey
       , registerEgest
       , registerRelay
       , status
       , stopAction
       , CachedState
       , ParentCallbacks
       , State
       , StateServerName
       ) where

import Prelude

import Bus as Bus
import Data.Array as Array
import Data.Either (Either(..), hush)
import Data.Foldable (find, foldl)
import Data.FoldableWithIndex (foldlWithIndex)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Maybe as Maybe
import Data.Newtype (un, unwrap)
import Data.Traversable (traverse)
import Data.TraversableWithIndex (traverseWithIndex)
import Data.Tuple as PursTuple
import Effect (Effect)
import Erl.Atom (Atom, atom)
import Erl.Data.List (List, nil, toUnfoldable, (:))
import Erl.Data.List as List
import Erl.Data.Map (Map)
import Erl.Data.Map as Map
import Erl.Data.Tuple as ErlTuple
import Erl.Process (Process(..), (!))
import Erl.Utils (Ref, makeRef)
import Erl.Utils as Erl
import Logger (Logger)
import Logger as Logger
import Partial.Unsafe as Unsafe
import Pinto (ServerName, StartLinkResult, isRegistered)
import Pinto.Gen (CallResult(..), CastResult(..), TerminateReason)
import Pinto.Gen as Gen
import Pinto.Timer as Timer
import Prim.Row as Row
import Rtsv2.Agents.CachedInstanceState as CachedInstanceState
import Rtsv2.Agents.IntraPoP (IntraPoPBusMessage(..))
import Rtsv2.Agents.IntraPoP as IntraPoP
import Rtsv2.Agents.SlotTypes (SlotConfiguration)
import Rtsv2.Agents.StreamRelayTypes (ActiveProfiles(..), CreateRelayPayload, DownstreamWsMessage(..), RelayUpstreamWsMessage(..), WebSocketHandlerMessage(..))
import Rtsv2.Agents.TransPoP as TransPoP
import Rtsv2.Config (StreamRelayConfig, LoadConfig)
import Rtsv2.Config as Config
import Rtsv2.DataObject as DO
import Rtsv2.Names as Names
import Rtsv2.PoPDefinition as PoPDefinition
import Rtsv2.Types (ResourceResp)
import Shared.Common (LoggingMetadata(..))
import Shared.Rtsv2.Agent (SlotCharacteristics)
import Shared.Rtsv2.Agent as Agent
import Shared.Rtsv2.Agent.State as PublicState
import Shared.Rtsv2.JsonLd as JsonLd
import Shared.Rtsv2.Router.Endpoint.System as Support
import Shared.Rtsv2.Router.Endpoint.System as System
import Shared.Rtsv2.Stream (AggregatorKey(..), ProfileName, RelayKey(..), SlotId(..), SlotRole, relayKeyToAggregatorKey)
import Shared.Rtsv2.Types (DeliverTo, EgestServer, OnBehalfOf(..), PoPName, RelayServer, Server, ServerAddress(..), SourceRoute, extractAddress)
import Shared.UUID (UUID)
import SpudGun (SpudResponse(..), StatusCode(..))
import SpudGun as SpudGun
import WsGun as WsGun

-- -----------------------------------------------------------------------------
-- FFI
-- -----------------------------------------------------------------------------
foreign import data WorkflowHandle :: Type

foreign import startOriginWorkflowFFI :: UUID -> Effect WorkflowHandle
foreign import applyOriginPlanFFI :: OriginStreamRelayPlan -> WorkflowHandle -> Effect OriginStreamRelayApplyResult

foreign import startDownstreamWorkflowFFI :: UUID -> Effect WorkflowHandle
foreign import applyDownstreamPlanFFI :: DownstreamStreamRelayPlan -> WorkflowHandle -> Effect DownstreamStreamRelayApplyResult

foreign import setSlotConfigurationFFI :: RelayKey -> SlotConfiguration -> Effect Unit
foreign import getSlotConfigurationFFI :: RelayKey -> Effect (Maybe SlotConfiguration)

foreign import stopWorkflowFFI :: WorkflowHandle -> Effect Unit

type ParentCallbacks
  = { startLocalOrRemoteStreamRelay :: LoadConfig -> OnBehalfOf -> CreateRelayPayload -> Effect (ResourceResp Server)
    }

-- -----------------------------------------------------------------------------
-- Gen Server State
-- -----------------------------------------------------------------------------
data CachedState = CachedOrigin CommonStateData OriginStreamRelayConfig (Maybe OriginStreamRelayRunState)
                 | CachedDownstream DownstreamStreamRelayConfig (Maybe DownstreamStreamRelayRunState)

type StateServerName = CachedInstanceState.StateServerName CachedState

data State
  = StateOrigin CommonStateData OriginStreamRelayStateData
  | StateDownstream CommonStateData DownstreamStreamRelayStateData

type CommonStateData =
  { relayKey :: RelayKey
  , parentCallbacks :: ParentCallbacks
  , thisServer :: Server
  , aggregatorPoP :: PoPName
  , slotCharacteristics :: SlotCharacteristics
  , loadConfig :: LoadConfig
  , stateServerName :: StateServerName
  , config :: StreamRelayConfig
  , stopRef :: Maybe Ref
  , dataObject :: Maybe DO.Object
  , activeProfiles :: List ProfileName
  , forceDrain :: Boolean
  , aggregatorExitTimerRef :: Maybe Ref
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
type EgestMap = Map ServerAddress { handler :: Process (WebSocketHandlerMessage DownstreamWsMessage)
                                  , deliverTo :: DeliverTo EgestServer
                                  }
type OriginRelayMap = Map ServerAddress { handler :: Process (WebSocketHandlerMessage DownstreamWsMessage)
                                        , deliverTo :: DeliverTo RelayServer
                                        }
type DownstreamRelayMap = Map ServerAddress { handler :: Process (WebSocketHandlerMessage DownstreamWsMessage)
                                            , deliverToWithSource :: DownstreamRelayWithSource
                                            }

type OriginStreamRelayConfig =
  { egests :: EgestMap
  , downstreamRelays :: OriginRelayMap
  }

type DownstreamStreamRelayConfig =
  { egestUpstreamRelays :: List UpstreamRelay
  , egests :: EgestMap
  , downstreamRelays :: DownstreamRelayMap
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
  , ingestAggregator :: Server
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
type WebSocket = WsGun.WebSocket RelayUpstreamWsMessage DownstreamWsMessage

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
  | UpstreamRelayStateRegistered PortNumber ServerAddress WebSocket
  | UpstreamRelayStatePendingDeregistration PortNumber ServerAddress WebSocket
  | UpstreamRelayStateDeregistered PortNumber

data IngestAggregatorState
  = IngestAggregatorStateDisabled
  | IngestAggregatorStatePendingRegistration PortNumber
  | IngestAggregatorStateRegistered PortNumber WebSocket

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
  , rest :: SourceRoute
  }

-- -----------------------------------------------------------------------------
-- API
-- -----------------------------------------------------------------------------
clearStopRef :: State -> State
clearStopRef (StateOrigin commonState runState) =
  StateOrigin commonState{stopRef = Nothing} runState
clearStopRef (StateDownstream commonState runState) =
  StateDownstream commonState{stopRef = Nothing} runState

registerEgest :: SlotId -> SlotRole -> EgestServer -> Int -> Process (WebSocketHandlerMessage DownstreamWsMessage)  -> Effect (Maybe SlotConfiguration)
registerEgest slotId slotRole egestServer egestPort handler@(Process handlerPid) =
  Gen.doCall (serverName $ RelayKey slotId slotRole) doRegisterEgest
  where
    doRegisterEgest :: State -> Effect (CallResult (Maybe SlotConfiguration) State)

    doRegisterEgest state@(StateOrigin {thisServer, dataObject, activeProfiles} { config: { egests }, run: { slotConfiguration: slotConfig } }) = do
      monitor
      maybeSendDataObject dataObject
      sendActiveProfiles activeProfiles
      CallReply slotConfig <$> clearStopRef <$> applyNewEgests (updateMap egests) state

    doRegisterEgest state@(StateDownstream {thisServer, dataObject, activeProfiles} { config: { egests }, run: { slotConfiguration: slotConfig } }) = do
      monitor
      maybeSendDataObject dataObject
      sendActiveProfiles activeProfiles
      CallReply slotConfig <$> clearStopRef <$> applyNewEgests (updateMap egests) state

    egestAddress = (unwrap egestServer).address
    deliverTo = { server: egestServer, port: egestPort }
    updateMap egests = Map.insert egestAddress { handler, deliverTo } egests
    monitor = Gen.monitorPid (serverName $ RelayKey slotId slotRole) handlerPid (\_ -> EgestDown egestAddress)
    maybeSendDataObject Nothing = pure unit
    maybeSendDataObject (Just dataObject) = do
      ref <- Erl.makeRef
      sendMessageToDownstream (DataObject $ DO.ObjectBroadcastMessage { object: dataObject
                                                                      , ref}) handler
    sendActiveProfiles activeProfiles = do
      ref <- Erl.makeRef
      sendMessageToDownstream (CurrentActiveProfiles $ ActiveProfiles { profiles: activeProfiles
                                                                      , ref}) handler

registerRelay :: SlotId -> SlotRole -> RelayServer -> Int -> SourceRoute -> (Process (WebSocketHandlerMessage DownstreamWsMessage)) -> Effect (Maybe SlotConfiguration)
registerRelay slotId slotRole relayServer relayPort sourceRoute handler@(Process handlerPid) =
  Gen.doCall (serverName $ RelayKey slotId slotRole) doRegisterRelay

  where
    doRegisterRelay :: State -> Effect(CallResult (Maybe SlotConfiguration) State)

    doRegisterRelay (StateOrigin commonStateData@{thisServer, dataObject, activeProfiles} originStateData@{ config: config@{ downstreamRelays }, run: { slotConfiguration: slotConfig } }) = do
      -- TODO: PS: log if we've got a non-nil source when we're in origin mode
      let
        newDownstreamRelays = Map.insert relayAddress { handler, deliverTo} downstreamRelays
      maybeSendDataObject dataObject
      sendActiveProfiles activeProfiles
      monitor
      CallReply slotConfig <$> clearStopRef <$> applyOriginNewRelays newDownstreamRelays commonStateData originStateData

    doRegisterRelay (StateDownstream commonStateData@{thisServer, dataObject, activeProfiles} downstreamStateData@{ config: config@{ downstreamRelays }, run: { slotConfiguration: slotConfig } }) =
      case Array.uncons sourceRoute of
        Just { head, tail } -> do
          let
            downstreamRelayWithSource =
              { source: { next: head, rest: tail }
              , deliverTo
              }
            newDownstreamRelays = Map.insert relayAddress { handler, deliverToWithSource: downstreamRelayWithSource} downstreamRelays
          maybeSendDataObject dataObject
          sendActiveProfiles activeProfiles
          monitor
          CallReply slotConfig <$> clearStopRef <$> applyDownstreamNewRelays newDownstreamRelays commonStateData downstreamStateData

        Nothing ->
          -- TODO: PS: log if we've got a nil source when we're in downstream mode
          pure $ CallReply Nothing $ StateDownstream commonStateData downstreamStateData

    relayAddress = (unwrap relayServer).address
    deliverTo = { server: relayServer, port: relayPort }
    monitor = Gen.monitorPid (serverName $ RelayKey slotId slotRole) handlerPid (\_ -> RelayDown relayAddress)
    maybeSendDataObject Nothing = pure unit
    maybeSendDataObject (Just dataObject) = do
      ref <- Erl.makeRef
      sendMessageToDownstream (DataObject $ DO.ObjectBroadcastMessage { object: dataObject
                                                                      , ref}) handler
    sendActiveProfiles activeProfiles = do
      ref <- Erl.makeRef
      sendMessageToDownstream (CurrentActiveProfiles $ ActiveProfiles { profiles: activeProfiles
                                                                      , ref}) handler

dataObjectSendMessage :: RelayKey -> DO.Message -> Effect Unit
dataObjectSendMessage relayKey msg =
  Gen.doCall (serverName relayKey)
  (\state -> do
    shouldProcess <- DO.shouldProcessMessage relayKey msg
    if shouldProcess then sendMessageToUpstreams (RelayUpstreamDataObjectMessage msg) state
    else pure unit
    pure $ CallReply unit state
  )

dataObjectUpdateSendMessage :: RelayKey -> DO.ObjectUpdateMessage -> Effect Unit
dataObjectUpdateSendMessage relayKey msg =
  Gen.doCall (serverName relayKey)
  (\state -> do
    shouldProcess <- DO.shouldProcessMessage relayKey msg
    if shouldProcess then sendMessageToUpstreams (RelayUpstreamDataObjectUpdateMessage msg) state
    else pure unit
    pure $ CallReply unit state
  )

forceDrain :: RelayKey -> Effect Unit
forceDrain relayKey =
  Gen.doCast (serverName relayKey) doForceDrain
  where
    doForceDrain (StateOrigin commonState runState) = do
      commonState2 <- doForceDrain' commonState
      pure $ CastNoReply $ StateOrigin commonState2 runState
    doForceDrain (StateDownstream commonState runState) = do
      commonState2 <- doForceDrain' commonState
      pure $ CastNoReply $ StateDownstream commonState2 runState

    doForceDrain' commonState@{relayKey: RelayKey slotId slotRole
                              , aggregatorPoP
                              , slotCharacteristics
                              , config: {forceDrainTimeoutMs}
                              , parentCallbacks: { startLocalOrRemoteStreamRelay }
                              , loadConfig} = do
      logInfo "Stream Relay entering force-drain mode" {relayKey}
      void $ Timer.sendAfter (serverName relayKey) forceDrainTimeoutMs ForceDrainTimeout
      void $ startLocalOrRemoteStreamRelay loadConfig LocalAgent {slotId, slotRole, aggregatorPoP, slotCharacteristics}
      pure commonState{forceDrain = true}

applyOriginPlan :: CommonStateData -> OriginStreamRelayStateData -> Effect State

applyOriginPlan commonStateData@{relayKey, stateServerName, config: {reApplyPlanTimeMs}} originStateData@{ config, plan: Nothing } = do
  _ <- Timer.sendAfter (serverName relayKey) reApplyPlanTimeMs ReApplyPlan
  CachedInstanceState.recordInstanceData stateServerName (CachedOrigin commonStateData config Nothing)
  pure $ StateOrigin commonStateData originStateData

applyOriginPlan commonStateData@{stateServerName} originStateData@{ config, plan: Just plan, run: runState, workflowHandle } =
  do
    applyResult <- applyOriginPlanFFI plan workflowHandle

    newRunState <- applyOriginRunResult commonStateData plan applyResult runState

    CachedInstanceState.recordInstanceData stateServerName (CachedOrigin commonStateData config (Just newRunState))

    pure $ StateOrigin commonStateData $ originStateData{ run = newRunState }

applyDownstreamPlan :: CommonStateData -> DownstreamStreamRelayStateData -> Effect State

applyDownstreamPlan commonStateData@{stateServerName} downstreamStateData@{ config, plan: Nothing } = do
  CachedInstanceState.recordInstanceData stateServerName (CachedDownstream config Nothing)
  pure $ StateDownstream commonStateData downstreamStateData

applyDownstreamPlan commonStateData@{stateServerName} downstreamStateData@{ config, plan: Just plan, run: runState, workflowHandle } =
  do
    applyResult <- applyDownstreamPlanFFI plan workflowHandle
    newRunState <- applyDownstreamRunResult commonStateData applyResult runState
    CachedInstanceState.recordInstanceData stateServerName (CachedDownstream config (Just newRunState))
    pure $ StateDownstream commonStateData $ downstreamStateData{ run = newRunState }

applyOriginRunResult :: CommonStateData -> OriginStreamRelayPlan -> OriginStreamRelayApplyResult -> OriginStreamRelayRunState -> Effect OriginStreamRelayRunState
applyOriginRunResult commonStateData@{ relayKey: relayKey@(RelayKey slotId slotRole), thisServer } plan@{ingestAggregator} applyResult runState =
  (pure runState)
    <#> mergeOriginApplyResult applyResult
    >>= maybeTryRegisterIngestAggregator

  where
    maybeTryRegisterIngestAggregator runStateIn@{ ingestAggregatorState: IngestAggregatorStateDisabled } = pure runStateIn
    maybeTryRegisterIngestAggregator runStateIn@{ ingestAggregatorState: IngestAggregatorStateRegistered _ _} = pure runStateIn
    maybeTryRegisterIngestAggregator runStateIn@{ ingestAggregatorState: IngestAggregatorStatePendingRegistration portNumber } = do
      wsUrl <- Support.makeWsUrl ingestAggregator $ Support.IngestAggregatorRegisteredRelayWs slotId slotRole (extractAddress thisServer) portNumber
      response <-  WsGun.openWebSocket (serverName relayKey) Gun wsUrl
      logInfo "Attempted registration with ingest aggregator." { slotId
                                                               , slotRole
                                                               , ingestAggregator
                                                               , portNumber
                                                               , wsUrl }
      case response of
        Right socket -> do
          pure runStateIn{ ingestAggregatorState = IngestAggregatorStateRegistered portNumber socket }

        _other -> do
          pure runStateIn

applyDownstreamRunResult :: CommonStateData -> DownstreamStreamRelayApplyResult -> DownstreamStreamRelayRunState -> Effect DownstreamStreamRelayRunState
applyDownstreamRunResult commonStateData@{ relayKey: relayKey@(RelayKey slotId slotRole)
                                         , aggregatorPoP
                                         , thisServer
                                         , slotCharacteristics
                                         , loadConfig } applyResult runState =
  (pure runState)
    <#> mergeDownstreamApplyResult applyResult
    >>= maybeTryRegisterUpstreamRelays

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

        UpstreamRelayStatePendingDeregistration portNumber serverAddress webSocket ->
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
            pure $ (UpstreamRelayStatePendingRegistration portNumber)

          Just relayAddress ->
            do
              maybeWebSocket <- registerWithSpecificRelay portNumber relayAddress rest

              _ <- (logInfo "Attempted registration with relay" { relayAddress, portNumber })

              case maybeWebSocket of
                Just webSocket ->
                  pure $ (UpstreamRelayStateRegistered portNumber relayAddress webSocket)
                Nothing ->
                  pure $ (UpstreamRelayStatePendingRegistration portNumber)

    ensureRelayInPoP pop = do
      maybeRandomServerInPoP <- PoPDefinition.getRandomServerInPoP pop
      case maybeRandomServerInPoP of
        Nothing ->
          pure Nothing
        Just randomServerInPoP -> do
          let payload = { slotId, slotRole, aggregatorPoP, slotCharacteristics } :: CreateRelayPayload
          url <- System.makeUrl randomServerInPoP System.RelayEnsureStartedE
          eitherResponse <- SpudGun.postJsonFollow url payload

          case eitherResponse of
            Right response@(SpudResponse (StatusCode statusCode) _headers _body) | statusCode >= 200 && statusCode < 300 ->
              pure $ extractServedByHeader response

            _other ->
              pure $ Nothing

    registerWithSpecificRelay portNumber chosenRelay remainingRoute = do
      wsUrl <- System.makeWsUrlAddr chosenRelay $ System.RelayRegisteredRelayWs slotId slotRole (extractAddress thisServer) portNumber remainingRoute
      webSocket <- WsGun.openWebSocket (serverName relayKey) Gun wsUrl
      pure $ hush webSocket

isInstanceAvailable :: RelayKey -> Effect Boolean
isInstanceAvailable relayKey = isRegistered (serverName relayKey)

status :: RelayKey -> Effect (PublicState.StreamRelay List)
status rk =
  Gen.doCall (serverName rk) mkStatus
  where
    mkStatus state@(StateOrigin {relayKey: RelayKey slotId slotRole, thisServer} originStateData) = do
      newState <- publicState
      json <- JsonLd.streamRelayStateNode slotId newState thisServer
      pure $ Gen.CallReply json state
      where
        publicState = do
          es <- traverse (JsonLd.egestServedLocationNode slotId slotRole) $ Map.keys originStateData.config.egests
          rs <- traverse (JsonLd.downstreamRelayLocationNode slotId slotRole <$> _.deliverTo) $ Map.values originStateData.config.downstreamRelays
          pure { role : slotRole
               , egestsServed : es
               , relaysServed : rs
               }
    mkStatus state@(StateDownstream {relayKey: RelayKey slotId slotRole, thisServer} downstreamStateData) = do
      newState <- publicState
      json <- JsonLd.streamRelayStateNode slotId newState thisServer
      pure $ Gen.CallReply json state
      where
        publicState = do
          es <- traverse (JsonLd.egestServedLocationNode slotId slotRole) $ Map.keys downstreamStateData.config.egests
          rs <- traverse (JsonLd.downstreamRelayLocationNode slotId slotRole <$> _.deliverTo <$> _.deliverToWithSource)$ Map.values downstreamStateData.config.downstreamRelays
          pure { role : slotRole
               , egestsServed : es
               , relaysServed : rs
               }

-- -----------------------------------------------------------------------------
-- gen Server Implementation
-- -----------------------------------------------------------------------------
data Msg = IntraPoPBus IntraPoP.IntraPoPBusMessage
         | Gun WsGun.GunMsg
         | EgestDown ServerAddress
         | RelayDown ServerAddress
         | MaybeStop Ref
         | ReApplyPlan
         | ForceDrainTimeout
         | AggregatorExitTimer Ref

payloadToRelayKey :: forall r. { slotId :: SlotId, slotRole :: SlotRole | r } -> RelayKey
payloadToRelayKey payload = RelayKey payload.slotId payload.slotRole

serverName :: RelayKey -> ServerName State Msg
serverName = Names.streamRelayInstanceName

startLink :: RelayKey -> ParentCallbacks -> CreateRelayPayload -> StateServerName -> Effect StartLinkResult
startLink relayKey parentCallbacks payload stateServerName =
  Gen.startLink (serverName relayKey) (init relayKey parentCallbacks payload stateServerName) handleInfo

stopAction :: RelayKey -> Maybe CachedState -> Effect Unit
stopAction relayKey@(RelayKey slotId slotRole) cachedState = do
  logStop "Stream Relay stopping" {relayKey}
  thisServer <- PoPDefinition.getThisServer
  deRegisterFromPeers cachedState
  IntraPoP.announceLocalRelayStopped relayKey
  where
    deRegisterFromPeers Nothing =
      pure unit

    deRegisterFromPeers (Just (CachedOrigin _ _ maybeRunState)) =
      deRegisterOrigin maybeRunState

    deRegisterFromPeers (Just (CachedDownstream _ maybeRunState)) =
      deRegisterUpstreams maybeRunState

    deRegisterOrigin (Just {ingestAggregatorState: IngestAggregatorStateRegistered _ webSocket}) =
      WsGun.closeWebSocket webSocket

    deRegisterOrigin _ =
      pure unit

    deRegisterUpstreams Nothing =
      pure unit

    deRegisterUpstreams (Just {upstreamRelayStates}) = do
      _ <- traverse deRegisterUpstream (Map.values upstreamRelayStates)
      pure unit

    deRegisterUpstream (UpstreamRelayStatePendingDeregistration _portNumber _relayAddress webSocket) =
      WsGun.closeWebSocket webSocket

    deRegisterUpstream (UpstreamRelayStateRegistered _portNumber _relayAddress webSocket) =
      WsGun.closeWebSocket webSocket

    deRegisterUpstream _ =
      pure unit

init :: RelayKey -> ParentCallbacks -> CreateRelayPayload -> StateServerName -> Effect State
init relayKey parentCallbacks payload@{slotId, slotRole, aggregatorPoP, slotCharacteristics} stateServerName = do
  Logger.addLoggerMetadata $ PerSlot { slotId, slotRole, slotName: Nothing}
  Gen.registerExternalMapping (serverName relayKey) (\m -> Gun <$> (WsGun.messageMapper m))
  thisServer <- PoPDefinition.getThisServer
  egestSourceRoutes <- TransPoP.routesTo aggregatorPoP
  streamRelayConfig <- Config.streamRelayConfig
  loadConfig <- Config.loadConfig
  IntraPoP.announceLocalRelayIsAvailable relayKey
  _ <- Bus.subscribe (serverName relayKey) IntraPoP.bus IntraPoPBus
  Gen.registerTerminate (serverName relayKey) terminate
  mConfig <- CachedInstanceState.getInstanceData stateServerName

  let
    commonStateData =
      { relayKey
      , thisServer
      , parentCallbacks
      , slotCharacteristics
      , loadConfig
      , aggregatorPoP
      , stateServerName
      , config: streamRelayConfig
      , stopRef: Nothing
      , dataObject: Nothing
      , activeProfiles: nil
      , forceDrain: false
      , aggregatorExitTimerRef: Nothing
      }

  if egestSourceRoutes == List.nil then
    do
      logStart (fromMaybe "Origin Relay Starting" ((const "Origin Relay Restarting") <$> mConfig)) {relayKey}
      workflowHandle <- startOriginWorkflowFFI (un SlotId slotId)
      let
        config = cachedConfigToOriginConfig mConfig
      monitorEgests config
      monitorRelays config

      let
        initialOriginStateData =
          { workflowHandle
          , config
          , plan: Nothing
          , run: { slotConfiguration: Nothing
                 , ingestAggregatorState: IngestAggregatorStateDisabled
                 }
          }

      newPlan <- originConfigToPlan commonStateData config
      let
        newOriginStateData = initialOriginStateData{ plan = newPlan }
      applyOriginPlan commonStateData newOriginStateData
  else
    do
      logStart (fromMaybe "Downstream Relay Starting" ((const "Downstream Relay Restarting") <$> mConfig)) {relayKey}
      let
        egestUpstreamRelays = map mkUpstreamRelay $ toUnfoldable <$> egestSourceRoutes

      workflowHandle <- startDownstreamWorkflowFFI (un SlotId slotId)
      let
        config = cachedConfigToDownstreamConfig mConfig egestUpstreamRelays
      monitorEgests config
      monitorRelays config

      let
        initialDownstreamStateData =
          { workflowHandle
          , config
          , plan: Nothing
          , run: { slotConfiguration: Nothing
                 , upstreamRelayStates: Map.empty
                 }
          }

      newPlan <- downstreamConfigToPlan commonStateData config
      let
        newDownstreamStateData = initialDownstreamStateData{ plan = Just newPlan }

      applyDownstreamPlan commonStateData newDownstreamStateData

  where
    cachedConfigToOriginConfig mConfig =
      case mConfig of
        Nothing -> emptyOriginConfig
        Just (CachedDownstream _ _) -> emptyOriginConfig
        Just (CachedOrigin _ cachedConfig _) -> cachedConfig

    cachedConfigToDownstreamConfig mConfig egestUpstreamRelays =
      case mConfig of
        Nothing -> emptyDownstreamConfig egestUpstreamRelays
        Just (CachedOrigin _ _ _) -> emptyDownstreamConfig egestUpstreamRelays
        Just (CachedDownstream cachedConfig _) -> cachedConfig

    emptyOriginConfig =
      { egests: Map.empty
      , downstreamRelays: Map.empty }

    emptyDownstreamConfig egestUpstreamRelays =
      { egestUpstreamRelays
      , egests: Map.empty
      , downstreamRelays: Map.empty }

    monitorEgest egestAddress {handler: Process pid} =
      Gen.monitorPid (serverName relayKey) pid (\_ -> EgestDown egestAddress)

    monitorEgests :: forall r. {egests :: EgestMap | r} -> Effect Unit
    monitorEgests {egests} = do
      _ <- traverseWithIndex monitorEgest egests
      pure unit

    monitorRelay :: forall r1. ServerAddress -> { handler :: Process (WebSocketHandlerMessage DownstreamWsMessage) | r1} -> Effect Unit
    monitorRelay relayAddress {handler: Process pid} =
      Gen.monitorPid (serverName relayKey) pid (\_ -> RelayDown relayAddress)

    monitorRelays :: forall r1 r2. { downstreamRelays :: Map ServerAddress { handler :: Process (WebSocketHandlerMessage DownstreamWsMessage)
                                                                           | r1}
                                   | r2 } -> Effect Unit
    monitorRelays {downstreamRelays} = do
      _ <- traverseWithIndex monitorRelay downstreamRelays
      pure unit

handleInfo :: Msg -> State -> Effect (CastResult State)
handleInfo msg state =
  case msg of

    ReApplyPlan
      | StateOrigin commonStateData originStateData@{config} <- state -> do
          newPlan <- originConfigToPlan commonStateData config
          CastNoReply <$> applyOriginPlan commonStateData originStateData{plan = newPlan}
      | StateDownstream commonStateData downstreamStateData <- state ->
        CastNoReply <$> applyDownstreamPlan commonStateData downstreamStateData
      | otherwise ->
        pure $ CastNoReply state -- todo - why clause needed?

    IntraPoPBus (IngestAggregatorStarted _ _) ->
      pure $ CastNoReply state

    IntraPoPBus (IngestAggregatorExited (AggregatorKey slotId slotRole) serverAddress)
      | slotId == ourSlotId && slotRole == ourSlotRole -> do
        ref <- Erl.makeRef
        case state of
          StateDownstream common@{config: {aggregatorExitLingerTimeMs}} runState -> do
            void $ Timer.sendAfter (serverName relayKey) aggregatorExitLingerTimeMs (AggregatorExitTimer ref)
            pure $ CastNoReply $ StateDownstream common{aggregatorExitTimerRef = Just ref} runState
          StateOrigin common@{config: {aggregatorExitLingerTimeMs}} runState -> do
            void $ Timer.sendAfter (serverName relayKey) aggregatorExitLingerTimeMs (AggregatorExitTimer ref)
            pure $ CastNoReply $ StateOrigin common{aggregatorExitTimerRef = Just ref} runState
      | otherwise -> pure $ CastNoReply state

    IntraPoPBus (StreamRelayStarted (RelayKey startedId startedRole) startedServer) | startedId == ourSlotId
                                                                                      && startedRole == ourSlotRole
                                                                                      && startedServer /= thisServer ->
      -- A twin just started on another server
      if isForceDrain state then do
        logInfo "ForceDrain mode: Twin just started; announcing that we have stopped" {relayKey}
        IntraPoP.announceLocalRelayStopped relayKey
        pure $ CastNoReply state
      else do
        -- Normal to have multiple relays
        pure $ CastNoReply state

    IntraPoPBus (StreamRelayStarted _ _) ->
      pure $ CastNoReply state

    IntraPoPBus (StreamRelayExited (RelayKey exitedId exitedRole) exitedServer) | exitedId == ourSlotId
                                                                                  && exitedRole == ourSlotRole
                                                                                  && exitedServer == thisServer ->
      -- We just exited!  Only makes sense in forceDrain mode where we announce stop prior to stopping
      if isForceDrain state then do
        logInfo "ForceDrain mode: Announcement of our exit; stopping" {relayKey}
        pure $ CastStop state
      else do
        logInfo "Normal mode: Unexpected announcement of our exit" {relayKey}
        pure $ CastNoReply state

    IntraPoPBus (StreamRelayExited _ _) ->
      pure $ CastNoReply state

    IntraPoPBus (EgestStarted _ _) ->
      pure $ CastNoReply state

    IntraPoPBus (EgestExited _ _) ->
      pure $ CastNoReply state

    IntraPoPBus (VmReset _ _ _) ->
      pure $ CastNoReply state

    ForceDrainTimeout -> do
      logInfo "Ingest Aggregator stopping due to force drain timeout" {relayKey}
      pure $ CastStop state

    AggregatorExitTimer ref -> maybeAggregatorStop state ref

    MaybeStop ref -> maybeStop state ref

    Gun inMsg ->
      processGunMessage state inMsg

    EgestDown egestAddress -> do
      _ <- logInfo "Egest down" {egestAddress}
      state2 <- fireStopTimer state
      CastNoReply <$> deRegisterEgest egestAddress state2

    RelayDown relayAddress -> do
      _ <- logInfo "Relay down" {relayAddress}
      state2 <- fireStopTimer state
      CastNoReply <$> deRegisterRelay relayAddress state2

  where
    isForceDrain (StateOrigin {forceDrain: true} _) = true
    isForceDrain (StateDownstream {forceDrain: true} _) = true
    isForceDrain _ = false

    thisServer = case state of
                   (StateOrigin { thisServer: this } _) -> this
                   (StateDownstream { thisServer: this } _) -> this

    relayKey = relayKeyFromState state
    (RelayKey ourSlotId ourSlotRole) = relayKey

    fireStopTimer (StateOrigin commonState runState) = do
      commonState2 <- fireStopTimer' commonState
      pure $ StateOrigin commonState2 runState
    fireStopTimer (StateDownstream commonState runState) = do
      commonState2 <- fireStopTimer' commonState
      pure $ StateDownstream commonState2 runState

    fireStopTimer' commonState@{ config : { lingerTimeMs } } = do
      ref <- makeRef
      _ <- Timer.sendAfter (serverName relayKey) lingerTimeMs (MaybeStop ref)
      pure commonState{stopRef = Just ref}

    maybeAggregatorStop (StateOrigin {aggregatorExitTimerRef} runState) ref
      | aggregatorExitTimerRef == Just ref = do
        mAggregator <- IntraPoP.whereIsIngestAggregator (relayKeyToAggregatorKey relayKey)
        case mAggregator of
          Nothing -> pure $ CastStop state
          Just _ -> pure $ CastNoReply state
      | otherwise = pure $ CastNoReply state

    maybeAggregatorStop (StateDownstream {aggregatorExitTimerRef} runState) ref
      | aggregatorExitTimerRef == Just ref = do
        mAggregator <- IntraPoP.whereIsIngestAggregator (relayKeyToAggregatorKey relayKey)
        case mAggregator of
          Nothing -> pure $ CastStop state
          Just _ -> pure $ CastNoReply state
      | otherwise = pure $ CastNoReply state

    maybeStop (StateOrigin {stopRef} {config: {downstreamRelays, egests}}) ref
      | (Map.isEmpty downstreamRelays) && (Map.isEmpty egests) && (Just ref == stopRef) = pure $ CastStop state
      | otherwise = pure $ CastNoReply state

    maybeStop (StateDownstream {stopRef} {config: {downstreamRelays, egests}}) ref
      | (Map.isEmpty downstreamRelays) && (Map.isEmpty egests) && (Just ref == stopRef) = pure $ CastStop state
      | otherwise = pure $ CastNoReply state

    deRegisterEgest egestAddress innerState@(StateOrigin _ { config: { egests } }) = do
      let
        newEgests = Map.delete egestAddress egests
      applyNewEgests newEgests innerState

    deRegisterEgest egestAddress innerState@(StateDownstream _ { config: { egests } }) = do
      let
        newEgests = Map.delete egestAddress egests
      applyNewEgests newEgests innerState

    deRegisterRelay relayAddress (StateOrigin commonStateData originStateData@{ config: { downstreamRelays } }) = do
      -- TODO: PS: log if we've got a non-nil source when we're in origin mode
      let
        newDownstreamRelays = Map.delete relayAddress downstreamRelays
      applyOriginNewRelays newDownstreamRelays commonStateData originStateData

    deRegisterRelay relayAddress (StateDownstream commonStateData downstreamStateData@{ config: { downstreamRelays } }) = do
      let
        newDownstreamRelays = Map.delete relayAddress downstreamRelays
      applyDownstreamNewRelays newDownstreamRelays commonStateData downstreamStateData

    findUpstreamSocket :: WsGun.GunMsg -> Map UpstreamRelay UpstreamRelayState -> Maybe { upstreamRelay :: UpstreamRelay, socket :: WebSocket, portNumber :: Int}
    findUpstreamSocket gunMsg map =
      List.head $ List.filter (\{socket} -> WsGun.isSocketForMessage gunMsg socket) $ List.catMaybes $ getSocket <$> Map.toUnfoldable map
      where
        getSocket (PursTuple.Tuple _ (UpstreamRelayStatePendingRegistration _portNumber)) = Nothing
        getSocket (PursTuple.Tuple upstreamRelay (UpstreamRelayStateRegistered portNumber _serverAddress socket)) = Just {upstreamRelay, socket, portNumber}
        getSocket (PursTuple.Tuple upstreamRelay (UpstreamRelayStatePendingDeregistration portNumber _serverAddress socket)) = Just {upstreamRelay, socket, portNumber}
        getSocket (PursTuple.Tuple _ (UpstreamRelayStateDeregistered _portNumber)) = Nothing

    findAggregatorSocket :: WsGun.GunMsg -> IngestAggregatorState -> Maybe { socket :: WebSocket, portNumber :: Int }
    findAggregatorSocket gunMsg IngestAggregatorStateDisabled = Nothing
    findAggregatorSocket gunMsg (IngestAggregatorStatePendingRegistration _) = Nothing
    findAggregatorSocket gunMsg (IngestAggregatorStateRegistered portNumber socket) = Just {socket, portNumber}

    sendMessageToDownstreams :: forall r1 r2 r3 egestKey relayKey. DownstreamWsMessage -> { egests :: Map egestKey { handler :: Process (WebSocketHandlerMessage DownstreamWsMessage) | r1 }
                                                                                    , downstreamRelays :: Map relayKey { handler :: Process (WebSocketHandlerMessage DownstreamWsMessage) | r2 } | r3 } -> Effect Unit
    sendMessageToDownstreams msg' {egests, downstreamRelays} = do
      let
        egestPids = Map.values egests <#> _.handler
        relayPids = Map.values downstreamRelays <#> _.handler
      _ <- traverse (sendMessageToDownstream msg') (egestPids <> relayPids)
      pure unit

    processGunMessage stateOrigin@(StateOrigin common@{config: {reApplyPlanTimeMs}} origin@{config, run: run@{ingestAggregatorState}}) gunMsg = do
      case findAggregatorSocket gunMsg ingestAggregatorState of
        Just {portNumber, socket} -> do
          let
            noop =
              pure stateOrigin

            down = do
              _ <- logInfo "Aggregator down, scheduling reapply" {reApplyPlanTimeMs}
              _ <- Timer.sendAfter (serverName relayKey) reApplyPlanTimeMs ReApplyPlan
              pure $ StateOrigin common origin{ run = run { ingestAggregatorState = IngestAggregatorStatePendingRegistration portNumber} }

            updateSocket newSocket = do
              pure $ StateOrigin common origin{ run = run { ingestAggregatorState = IngestAggregatorStateRegistered portNumber newSocket} }

            slotConfig slotConfiguration
              | Nothing <- run.slotConfiguration = do
                _ <- logInfo "Received slot configuration" {slotConfiguration}
                setSlotConfigurationFFI (relayKeyFromState state) slotConfiguration
                sendMessageToDownstreams (SlotConfig slotConfiguration) config
                pure $ StateOrigin common origin{ run = run { slotConfiguration = Just slotConfiguration }}
              | otherwise =
                pure stateOrigin

            send msgD@(DataObject (DO.ObjectBroadcastMessage {object})) = do
              sendMessageToDownstreams msgD config
              pure $ StateOrigin common{dataObject = Just object} origin

            send msgC@(CurrentActiveProfiles (ActiveProfiles {profiles})) = do
              sendMessageToDownstreams msgC config
              pure $ StateOrigin common{activeProfiles = profiles} origin

            send msg' = do
              sendMessageToDownstreams msg' config
              pure stateOrigin

          CastNoReply <$> processGunMessage' noop down updateSocket slotConfig send socket gunMsg

        Nothing ->
          pure $ CastNoReply stateOrigin

    processGunMessage stateDownStream@(StateDownstream common@{config: {reApplyPlanTimeMs}} downstream@{config, run: run@{upstreamRelayStates}}) gunMsg =
      case findUpstreamSocket gunMsg upstreamRelayStates of
        Just {upstreamRelay, socket, portNumber} -> do
          let
            noop =
              pure stateDownStream

            down = do
              _ <- logInfo "Upstream Relay down, scheduling replay" {upstreamRelay, reApplyPlanTimeMs}
              _ <- Timer.sendAfter (serverName relayKey) reApplyPlanTimeMs ReApplyPlan
              let
                newRelayStates = Map.insert upstreamRelay (UpstreamRelayStatePendingRegistration portNumber) upstreamRelayStates
              pure $ StateDownstream common downstream{run = run{ upstreamRelayStates = newRelayStates } }

            updateSocket newSocket = do
              let
                updateSocket' x@(UpstreamRelayStatePendingRegistration _portNumber) = Just x
                updateSocket' x@(UpstreamRelayStateRegistered portNumber serverAddress _socket) = Just $ UpstreamRelayStateRegistered portNumber serverAddress newSocket
                updateSocket' x@(UpstreamRelayStatePendingDeregistration portNumber serverAddress _socket) = Just $ UpstreamRelayStatePendingDeregistration portNumber serverAddress newSocket
                updateSocket' x@(UpstreamRelayStateDeregistered _portNumber) = Just x
                newRelayStates = Map.update updateSocket' upstreamRelay upstreamRelayStates
              pure $ StateDownstream common downstream{run = run{ upstreamRelayStates = newRelayStates } }

            slotConfig slotConfiguration
              | Nothing <- run.slotConfiguration = do
                _ <- logInfo "Received slot configuration" {slotConfiguration}
                setSlotConfigurationFFI (relayKeyFromState state) slotConfiguration
                sendMessageToDownstreams (SlotConfig slotConfiguration) config
                pure $ StateDownstream common downstream{run = run{ slotConfiguration = Just slotConfiguration }}
              | otherwise =
                pure stateDownStream

            send msgD@(DataObject (DO.ObjectBroadcastMessage {object})) = do
              sendMessageToDownstreams msgD config
              pure $ StateDownstream common{dataObject = Just object} downstream

            send msgC@(CurrentActiveProfiles (ActiveProfiles {profiles})) = do
              sendMessageToDownstreams msgC config
              pure $ StateDownstream common{activeProfiles = profiles} downstream

            send msg' = do
              sendMessageToDownstreams msg' config
              pure stateDownStream

          CastNoReply <$> processGunMessage' noop down updateSocket slotConfig send socket gunMsg

        Nothing ->
          pure $ CastNoReply stateDownStream

    processGunMessage' :: forall a. Effect a -> Effect a -> (WebSocket -> Effect a) -> (SlotConfiguration -> Effect a) -> (DownstreamWsMessage -> Effect a) -> WebSocket -> WsGun.GunMsg -> Effect a
    processGunMessage' noop down updateSocket slotConfig send socket gunMsg = do
      processResponse <- WsGun.processMessage socket gunMsg
      case processResponse of
        Left error -> do
          _ <- logInfo "Gun process error" {error}
          noop

        Right (WsGun.Internal _) ->
          noop

        Right WsGun.WebSocketUp -> do
          noop

        Right (WsGun.WebSocketUpdate newSocket) ->
          updateSocket newSocket

        Right WsGun.WebSocketDown -> do
          -- todo - kick off timer?  If websocket doesn't recover, attempt to launch new relay?
          _ <- logInfo "Relay Websocket down - but which one!!" {socket}
          down

        Right (WsGun.Frame (SlotConfig slotConfiguration)) ->
          slotConfig slotConfiguration

        Right (WsGun.Frame onFI@(OnFI _)) -> do
          send onFI

        Right (WsGun.Frame activeProfiles@(CurrentActiveProfiles _profiles)) -> do
          send activeProfiles

        Right (WsGun.Frame dataObjectMsg@(DataObjectMessage _msg)) -> do
          send dataObjectMsg

        Right (WsGun.Frame dataObjectMsg@(DataObjectUpdateResponse _msg)) -> do
          send dataObjectMsg

        Right (WsGun.Frame dataObjectMsg@(DataObject _msg)) -> do
          send dataObjectMsg

terminate :: TerminateReason -> State -> Effect Unit
terminate reason state = do
  logInfo "Stream Relay terminating" {reason}
  stopWorkflow state
  pure unit

stopWorkflow :: State -> Effect Unit
stopWorkflow (StateOrigin _ {workflowHandle}) = do
  stopWorkflowFFI workflowHandle
  pure unit
stopWorkflow (StateDownstream _ {workflowHandle}) = do
  stopWorkflowFFI workflowHandle
  pure unit

sendMessageToUpstreams :: RelayUpstreamWsMessage -> State -> Effect Unit
sendMessageToUpstreams msg (StateOrigin _ {run: { ingestAggregatorState } }) = do
  sendUpstream ingestAggregatorState
  pure unit
  where
    sendUpstream IngestAggregatorStateDisabled = pure unit
    sendUpstream (IngestAggregatorStatePendingRegistration _portNumber) = pure unit
    sendUpstream (IngestAggregatorStateRegistered _portNumber socket) = WsGun.send socket msg

sendMessageToUpstreams msg (StateDownstream _ {run: { upstreamRelayStates } }) = do
  void $ traverse sendUpstream $ Map.values upstreamRelayStates
  pure unit
  where
    sendUpstream (UpstreamRelayStatePendingRegistration _portNumber) = pure unit
    sendUpstream (UpstreamRelayStateRegistered _portNumber _serverAddress socket) = WsGun.send socket msg
    sendUpstream (UpstreamRelayStatePendingDeregistration _portNumber _serverAddress socket) = WsGun.send socket msg
    sendUpstream (UpstreamRelayStateDeregistered _portNumber) = pure unit

-- -----------------------------------------------------------------------------
-- Config -> Plan Transformation
-- -----------------------------------------------------------------------------
originConfigToPlan :: CommonStateData -> OriginStreamRelayConfig -> Effect (Maybe OriginStreamRelayPlan)
originConfigToPlan { relayKey } config@{ egests, downstreamRelays } = do
  maybeAggregator <- IntraPoP.whereIsIngestAggregator (relayKeyToAggregatorKey relayKey)
  pure $ { egests: plannedEgests
         , downstreamRelays: plannedDownstreamRelays
         , ingestAggregator: _
         } <$> maybeAggregator
  where
    plannedEgests = egests # Map.values # map _.deliverTo # map deliverToAddressFromDeliverToEgestServer
    plannedDownstreamRelays = downstreamRelays # Map.values <#> _.deliverTo # map deliverToAddressFromDeliverToRelayServer

downstreamConfigToPlan :: CommonStateData -> DownstreamStreamRelayConfig -> Effect DownstreamStreamRelayPlan
downstreamConfigToPlan { stateServerName } config@{ egestUpstreamRelays } = do
  pure { egests: plannedEgests
       , downstreamRelays: plannedDownstreamRelays
       , upstreamRelaySources
       }
  where
    egestList = config.egests # Map.values

    plannedEgests = egestList <#> _.deliverTo <#> deliverToAddressFromDeliverToEgestServer

    plannedDownstreamRelays = downstreamRelayList <#> _.deliverToWithSource # map (_.deliverTo) # map deliverToAddressFromDeliverToRelayServer

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
        <#> _.deliverToWithSource
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

            IngestAggregatorStateRegistered _portNumber _socket ->
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

            UpstreamRelayStateRegistered portNumber serverAddress webSocket ->
              -- It was previously registered, unregister it
              UpstreamRelayStatePendingDeregistration portNumber serverAddress webSocket

            UpstreamRelayStatePendingDeregistration _portNumber _serverAddress _webSocket ->
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

            UpstreamRelayStateRegistered existingPortNumber existingServerAddress _webSocket ->
              -- TODO: PS: validate the port number matches?
              relayStatesIn

            UpstreamRelayStatePendingDeregistration _portNumber _serverAddress _webSocket ->

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

applyNewEgests :: EgestMap -> State -> Effect State
applyNewEgests newEgests (StateOrigin commonStateData@{thisServer} originStateData@{config}) = do
  let
    newConfig = config{ egests = newEgests }
  newPlan <- originConfigToPlan commonStateData newConfig
  let
    newOriginStateData = originStateData{ config = newConfig, plan = newPlan }
  applyOriginPlan commonStateData newOriginStateData

applyNewEgests newEgests (StateDownstream commonStateData@{thisServer} downstreamStateData@{config}) = do
  let
    newConfig = config{ egests = newEgests }
  newPlan <- downstreamConfigToPlan commonStateData newConfig
  let
    newDownstreamStateData = downstreamStateData{ config = newConfig, plan = Just newPlan }
  applyDownstreamPlan commonStateData newDownstreamStateData

applyOriginNewRelays :: OriginRelayMap -> CommonStateData -> OriginStreamRelayStateData -> Effect State
applyOriginNewRelays newDownstreamRelays commonStateData originStateData@{ config } = do
  let
    newConfig = config{ downstreamRelays = newDownstreamRelays }
  newPlan <- originConfigToPlan commonStateData newConfig
  let
    newOriginStateData = originStateData{ config = newConfig, plan = newPlan }
  applyOriginPlan commonStateData newOriginStateData

applyDownstreamNewRelays :: DownstreamRelayMap -> CommonStateData -> DownstreamStreamRelayStateData -> Effect State
applyDownstreamNewRelays newDownstreamRelays commonStateData downstreamStateData@{ config } = do
  let
    newConfig = config{ downstreamRelays = newDownstreamRelays }
  newPlan <- downstreamConfigToPlan commonStateData newConfig
  let
    newDownstreamStateData = downstreamStateData{ config = newConfig, plan = Just newPlan }
  applyDownstreamPlan commonStateData newDownstreamStateData

mkUpstreamRelay :: SourceRoute -> UpstreamRelay
mkUpstreamRelay route =
  let
    { head: next, tail: rest } = Unsafe.unsafePartial $ Maybe.fromJust $ Array.uncons route
  in
    { next, rest }

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

sendMessageToDownstream :: DownstreamWsMessage -> (Process (WebSocketHandlerMessage DownstreamWsMessage)) -> Effect Unit
sendMessageToDownstream msg process =
  process ! (WsSend msg)


--------------------------------------------------------------------------------
-- Log Utilities
--------------------------------------------------------------------------------
domain :: List Atom
domain = atom <$> (show Agent.StreamRelay :  "Instance" : List.nil)

logInfo :: forall report. Row.Lacks "text" report => String -> { | report } -> Effect Unit
logInfo = Logger.doLog domain Logger.info

logWarning :: forall report. Row.Lacks "text" report => String -> { | report } -> Effect Unit
logWarning = Logger.doLog domain Logger.warning

logError :: forall report. Row.Lacks "text" report => String -> { | report } -> Effect Unit
logError = Logger.doLog domain Logger.error

logStart :: forall report. Row.Lacks "text" report => String -> { | report } -> Effect Unit
logStart = Logger.doLogEvent domain Logger.Start Logger.info

logStop :: forall report. Row.Lacks "text" report => String -> { | report } -> Effect Unit
logStop = Logger.doLogEvent domain Logger.Stop Logger.info
