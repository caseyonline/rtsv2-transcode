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
       , stopAction
       , isInstanceAvailable
       , status
       , registerEgest
       , registerRelay
       , init
       , State

       , payloadToRelayKey
       , domain
       , CachedState
       , StateServerName
       ) where

import Prelude

import Bus as Bus
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (find, foldl)
import Data.FoldableWithIndex (foldlWithIndex)
import Data.Int (round)
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Data.Newtype (un, unwrap)
import Data.Traversable (traverse)
import Data.TraversableWithIndex (traverseWithIndex)
import Data.Tuple as PursTuple
import Effect (Effect)
import Erl.Atom (Atom, atom)
import Erl.Data.List (List, toUnfoldable, (:))
import Erl.Data.List as List
import Erl.Data.Map (Map)
import Erl.Data.Map as Map
import Erl.Data.Tuple as ErlTuple
import Erl.Process (Process(..), (!))
import Erl.Utils (Ref, makeRef)
import Foreign (Foreign)
import Logger (Logger, spy)
import Logger as Logger
import Partial.Unsafe as Unsafe
import Pinto (ServerName, StartLinkResult, isRegistered)
import Pinto.Gen (CallResult(..), CastResult(..))
import Pinto.Gen as Gen
import Pinto.Timer as Timer
import PintoHelper (exposeState)
import Rtsv2.Agents.CachedInstanceState as CachedInstanceState
import Rtsv2.Agents.IntraPoP (IntraPoPBusMessage(..))
import Rtsv2.Agents.IntraPoP as IntraPoP
import Rtsv2.Agents.SlotTypes (SlotConfiguration)
import Rtsv2.Agents.StreamRelayTypes (CreateRelayPayload, DownstreamWsMessage(..), RelayToRelayClientWsMessage, WebSocketHandlerMessage(..))
import Rtsv2.Agents.TransPoP as TransPoP
import Rtsv2.Config (StreamRelayConfig)
import Rtsv2.Config as Config
import Rtsv2.Names as Names
import Rtsv2.PoPDefinition as PoPDefinition
import Rtsv2.Utils (crashIfLeft)
import Shared.Agent as Agent
import Shared.Common (Milliseconds(..))
import Shared.Router.Endpoint (Endpoint(..), makeUrlAddr, makeWsUrl, makeWsUrlAddr)
import Shared.Rtsv2.JsonLd as JsonLd
import Shared.Stream (AggregatorKey(..), RelayKey(..), SlotId(..), SlotRole)
import Shared.Types (PoPName, DeliverTo, EgestServer, RelayServer(..), Server(..), ServerAddress(..), SourceRoute, extractPoP, extractAddress)
import Shared.Types.Agent.State as PublicState
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
  , thisServer :: Server
  , ingestAggregator :: Server
  , stateServerName :: StateServerName
  , config :: StreamRelayConfig
  , stopRef :: Maybe Ref
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
type EgestMap = Map ServerAddress { handler :: Process WebSocketHandlerMessage
                                  , deliverTo :: DeliverTo EgestServer
                                  }
type OriginRelayMap = Map ServerAddress { handler :: Process WebSocketHandlerMessage
                                        , deliverTo :: DeliverTo RelayServer
                                        }
type DownstreamRelayMap = Map ServerAddress { handler :: Process WebSocketHandlerMessage
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
type WebSocket = WsGun.WebSocket RelayToRelayClientWsMessage DownstreamWsMessage

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

registerEgest :: SlotId -> SlotRole -> EgestServer -> Int -> Process WebSocketHandlerMessage -> Effect (Maybe SlotConfiguration)
registerEgest slotId slotRole egestServer egestPort handler@(Process handlerPid) =
  Gen.doCall (serverName $ RelayKey slotId slotRole) doRegisterEgest
  where
    doRegisterEgest :: State -> Effect (CallResult (Maybe SlotConfiguration) State)

    doRegisterEgest state@(StateOrigin {thisServer} { config: { egests }, run: { slotConfiguration: slotConfig } }) = do
      monitor
      CallReply slotConfig <$> clearStopRef <$> applyNewEgests (updateMap egests) state

    doRegisterEgest state@(StateDownstream {thisServer} { config: { egests }, run: { slotConfiguration: slotConfig } }) = do
      monitor
      CallReply slotConfig <$> clearStopRef <$> applyNewEgests (updateMap egests) state

    egestAddress = (unwrap egestServer).address
    deliverTo = { server: egestServer, port: egestPort }
    updateMap egests = Map.insert egestAddress { handler, deliverTo } egests
    monitor = Gen.monitorPid (serverName $ RelayKey slotId slotRole) handlerPid (\_ -> EgestDown egestAddress)

registerRelay :: SlotId -> SlotRole -> RelayServer -> Int -> SourceRoute -> (Process WebSocketHandlerMessage) -> Effect (Maybe SlotConfiguration)
registerRelay slotId slotRole relayServer relayPort sourceRoute handler@(Process handlerPid) =
  Gen.doCall (serverName $ RelayKey slotId slotRole) doRegisterRelay

  where
    doRegisterRelay :: State -> Effect(CallResult (Maybe SlotConfiguration) State)

    doRegisterRelay (StateOrigin commonStateData@{thisServer} originStateData@{ config: config@{ downstreamRelays }, run: { slotConfiguration: slotConfig } }) = do
      -- TODO: PS: log if we've got a non-nil source when we're in origin mode
      let
        newDownstreamRelays = Map.insert relayAddress { handler, deliverTo} downstreamRelays
      monitor
      CallReply slotConfig <$> clearStopRef <$> applyOriginNewRelays newDownstreamRelays commonStateData originStateData

    doRegisterRelay (StateDownstream commonStateData@{thisServer} downstreamStateData@{ config: config@{ downstreamRelays }, run: { slotConfiguration: slotConfig } }) =
      case Array.uncons sourceRoute of
        Just { head, tail } -> do
          let
            downstreamRelayWithSource =
              { source: { next: head, rest: tail }
              , deliverTo
              }
            newDownstreamRelays = Map.insert relayAddress { handler, deliverToWithSource: downstreamRelayWithSource} downstreamRelays
          monitor
          CallReply slotConfig <$> clearStopRef <$> applyDownstreamNewRelays newDownstreamRelays commonStateData downstreamStateData

        Nothing ->
          -- TODO: PS: log if we've got a nil source when we're in downstream mode
          pure $ CallReply Nothing $ StateDownstream commonStateData downstreamStateData

    relayAddress = (unwrap relayServer).address
    deliverTo = { server: relayServer, port: relayPort }
    monitor = Gen.monitorPid (serverName $ RelayKey slotId slotRole) handlerPid (\_ -> RelayDown relayAddress)

applyOriginPlan :: CommonStateData -> OriginStreamRelayStateData -> Effect State

applyOriginPlan commonStateData@{stateServerName} originStateData@{ config, plan: Nothing } = do
  CachedInstanceState.recordInstanceData stateServerName (CachedOrigin commonStateData config Nothing)
  pure $ StateOrigin commonStateData originStateData

applyOriginPlan commonStateData@{stateServerName} originStateData@{ config, plan: Just plan, run: runState, workflowHandle } =
  do
    applyResult <- applyOriginPlanFFI plan workflowHandle

    newRunState <- applyOriginRunResult commonStateData applyResult runState

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

applyOriginRunResult :: CommonStateData -> OriginStreamRelayApplyResult -> OriginStreamRelayRunState -> Effect OriginStreamRelayRunState
applyOriginRunResult commonStateData@{ relayKey: relayKey@(RelayKey slotId slotRole), thisServer, ingestAggregator } applyResult runState =
  (pure runState)
    <#> mergeOriginApplyResult applyResult
    >>= maybeTryRegisterIngestAggregator

  where
    maybeTryRegisterIngestAggregator runStateIn@{ ingestAggregatorState: IngestAggregatorStateDisabled } = pure runStateIn
    maybeTryRegisterIngestAggregator runStateIn@{ ingestAggregatorState: IngestAggregatorStateRegistered _ _} = pure runStateIn
    maybeTryRegisterIngestAggregator runStateIn@{ ingestAggregatorState: IngestAggregatorStatePendingRegistration portNumber } =
      let
        wsUrl = makeWsUrl ingestAggregator $ IngestAggregatorRegisteredRelayWs slotId slotRole (extractAddress thisServer) portNumber
      in
        do
           response <-  WsGun.openWebSocket wsUrl

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
applyDownstreamRunResult commonStateData@{ relayKey: relayKey@(RelayKey slotId slotRole), thisServer, ingestAggregator } applyResult runState =
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
              webSocket <- registerWithSpecificRelay portNumber relayAddress rest

              _ <- (logInfo "Attempted registration with relay" { relayAddress, portNumber })

              pure $ (UpstreamRelayStateRegistered portNumber relayAddress webSocket)

    ensureRelayInPoP pop =
      do
        maybeRandomServerInPoP <- PoPDefinition.getRandomServerInPoP pop

        case maybeRandomServerInPoP of
          Nothing ->
            pure Nothing

          Just randomServerInPoP ->
            let
              payload = { slotId, slotRole, aggregator: ingestAggregator } :: CreateRelayPayload
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
          , slotRole
          , deliverTo
          , sourceRoute: remainingRoute
          }

        wsUrl = makeWsUrlAddr chosenRelay $ RelayRegisteredRelayWs slotId slotRole (extractAddress thisServer) portNumber remainingRoute
      in
        do
          webSocket <- crashIfLeft =<< WsGun.openWebSocket wsUrl
          pure webSocket

isInstanceAvailable :: RelayKey -> Effect Boolean
isInstanceAvailable relayKey = isRegistered (serverName relayKey)

status :: RelayKey -> Effect (PublicState.StreamRelay List)
status =
  exposeState mkStatus <<< serverName
  where
    mkStatus (StateOrigin {relayKey: RelayKey slotId slotRole, thisServer} originStateData) =
      JsonLd.streamRelayStateNode slotId publicState thisServer
      where
        publicState =
          { role : slotRole
          , egestsServed : JsonLd.egestServedLocationNode slotId slotRole <$> Map.keys originStateData.config.egests
          , relaysServed : JsonLd.downstreamRelayLocationNode slotId slotRole <$> _.deliverTo <$> Map.values originStateData.config.downstreamRelays
          }
    mkStatus (StateDownstream {relayKey: RelayKey slotId slotRole, thisServer} downstreamStateData) =
      JsonLd.streamRelayStateNode slotId publicState thisServer
      where
        publicState =
          { role : slotRole
          , egestsServed : JsonLd.egestServedLocationNode slotId slotRole <$> Map.keys downstreamStateData.config.egests
          , relaysServed : JsonLd.downstreamRelayLocationNode slotId slotRole <$> _.deliverTo <$> _.deliverToWithSource <$> Map.values downstreamStateData.config.downstreamRelays
          }

-- -----------------------------------------------------------------------------
-- gen Server Implementation
-- -----------------------------------------------------------------------------
data Msg = IntraPoPBus IntraPoP.IntraPoPBusMessage
         | Gun WsGun.GunMsg
         | EgestDown ServerAddress
         | RelayDown ServerAddress
         | MaybeStop Ref

payloadToRelayKey :: forall r. { slotId :: SlotId, slotRole :: SlotRole | r } -> RelayKey
payloadToRelayKey payload = RelayKey payload.slotId payload.slotRole

serverName :: RelayKey -> ServerName State Msg
serverName = Names.streamRelayInstanceName

startLink :: RelayKey ->  CreateRelayPayload -> StateServerName -> Effect StartLinkResult
startLink relayKey payload stateServerName =
  Gen.startLink (serverName relayKey) (init relayKey payload stateServerName) handleInfo

stopAction :: RelayKey -> Maybe CachedState -> Effect Unit
stopAction relayKey@(RelayKey slotId slotRole) cachedState = do
  logInfo "Stream Relay stopping" {relayKey}
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

init :: RelayKey -> CreateRelayPayload -> StateServerName -> Effect State
init relayKey payload@{slotId, slotRole, aggregator} stateServerName =
  do
    Gen.registerExternalMapping (serverName relayKey) (\m -> Gun <$> (WsGun.messageMapper m))
    thisServer <- PoPDefinition.getThisServer
    egestSourceRoutes <- TransPoP.routesTo (extractPoP aggregator)
    streamRelayConfig <- Config.streamRelayConfig
    IntraPoP.announceLocalRelayIsAvailable relayKey
    _ <- Bus.subscribe (serverName relayKey) IntraPoP.bus IntraPoPBus
    Gen.registerTerminate (serverName relayKey) terminate

    let
      commonStateData =
        { relayKey
        , thisServer
        , ingestAggregator: aggregator
        , stateServerName
        , config: streamRelayConfig
        , stopRef: Nothing
        }

    if egestSourceRoutes == List.nil then
      do
        workflowHandle <- startOriginWorkflowFFI (un SlotId slotId)
        config <- getCachedOriginConfig
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
          newOriginStateData = initialOriginStateData{ plan = Just newPlan }
        applyOriginPlan commonStateData newOriginStateData
    else
      do
        let
          egestUpstreamRelays = map mkUpstreamRelay $ toUnfoldable <$> egestSourceRoutes

        workflowHandle <- startDownstreamWorkflowFFI (un SlotId slotId)
        config <- getCachedDownstreamConfig egestUpstreamRelays
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
    getCachedOriginConfig =
      case _ of
        Nothing -> emptyOriginConfig
        Just (CachedDownstream _ _) -> emptyOriginConfig
        Just (CachedOrigin _ cachedConfig _) -> cachedConfig
      <$> CachedInstanceState.getInstanceData stateServerName

    getCachedDownstreamConfig egestUpstreamRelays =
      case _ of
        Nothing -> emptyDownstreamConfig egestUpstreamRelays
        Just (CachedOrigin _ _ _) -> emptyDownstreamConfig egestUpstreamRelays
        Just (CachedDownstream cachedConfig _) -> cachedConfig
      <$> CachedInstanceState.getInstanceData stateServerName

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

    monitorRelay :: forall r1. ServerAddress -> { handler :: Process WebSocketHandlerMessage | r1} -> Effect Unit
    monitorRelay relayAddress {handler: Process pid} =
      Gen.monitorPid (serverName relayKey) pid (\_ -> RelayDown relayAddress)

    monitorRelays :: forall r1 r2. { downstreamRelays :: Map ServerAddress { handler :: Process WebSocketHandlerMessage
                                                                           | r1}
                                   | r2 } -> Effect Unit
    monitorRelays {downstreamRelays} = do
      _ <- traverseWithIndex monitorRelay downstreamRelays
      pure unit

handleInfo :: Msg -> State -> Effect (CastResult State)
handleInfo msg state =
  case msg of

    IntraPoPBus (IngestAggregatorExited (AggregatorKey slotId slotRole) serverAddress)
     -- TODO - PRIMARY BACKUP
      | slotId == ourSlotId -> pure $ CastStop state
      | otherwise -> pure $ CastNoReply state

    IntraPoPBus (VmReset _ _ _) ->
      pure $ CastNoReply state

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
    relayKey = relayKeyFromState state
    (RelayKey ourSlotId _) = relayKey

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

    sendSlotConfiguration :: SlotConfiguration -> (Process WebSocketHandlerMessage) -> Effect Unit
    sendSlotConfiguration slotConfiguration process =
      process ! (WsSend $ SlotConfig slotConfiguration)

    maybeSendSlotConfiguration :: forall r1 r2 r3 egestKey relayKey. Maybe SlotConfiguration -> Maybe SlotConfiguration -> {egests :: Map egestKey { handler :: Process WebSocketHandlerMessage | r1 },
                                                                                                                                                    downstreamRelays :: Map relayKey { handler :: Process WebSocketHandlerMessage | r2 } | r3 } -> Effect Unit
    maybeSendSlotConfiguration Nothing (Just newSlotConfiguration) {egests, downstreamRelays} = do
      let
        egestPids = Map.values egests <#> _.handler
        relayPids = Map.values downstreamRelays <#> _.handler
      _ <- traverse (sendSlotConfiguration newSlotConfiguration) (egestPids <> relayPids)
      pure unit
    maybeSendSlotConfiguration _oldRun _newRun _config =
      pure unit

    sendSlotConfigurationToPeers :: forall r1 r2 r3 egestKey relayKey. SlotConfiguration -> { egests :: Map egestKey { handler :: Process WebSocketHandlerMessage | r1 }
                                                                                            , downstreamRelays :: Map relayKey { handler :: Process WebSocketHandlerMessage | r2 } | r3 } -> Effect Unit
    sendSlotConfigurationToPeers newSlotConfiguration {egests, downstreamRelays} = do
      let
        egestPids = Map.values egests <#> _.handler
        relayPids = Map.values downstreamRelays <#> _.handler
      _ <- traverse (sendSlotConfiguration newSlotConfiguration) (egestPids <> relayPids)
      pure unit

    processGunMessage state@(StateOrigin common origin@{config, run: run@{ingestAggregatorState}}) gunMsg = do
      case findAggregatorSocket gunMsg ingestAggregatorState of
        Just {portNumber, socket} -> do
          let
            noop =
              pure state

            down =
              applyOriginPlan common origin{ run = run { ingestAggregatorState = IngestAggregatorStatePendingRegistration portNumber} }

            slotConfig slotConfiguration
              | Nothing <- run.slotConfiguration = do
                _ <- logInfo "Received slot configuration" {slotConfiguration}
                setSlotConfigurationFFI (relayKeyFromState state) slotConfiguration
                sendSlotConfigurationToPeers slotConfiguration config
                pure $ StateOrigin common origin{ run = run { slotConfiguration = Just slotConfiguration }}
              | otherwise =
                pure state

          CastNoReply <$> processGunMessage' noop down slotConfig socket gunMsg

        Nothing ->
          pure $ CastNoReply state

    processGunMessage state@(StateDownstream common downstream@{config, run: run@{upstreamRelayStates}}) gunMsg =
      case findUpstreamSocket gunMsg upstreamRelayStates of
        Just {upstreamRelay, socket, portNumber} -> do
          let
            noop =
              pure state

            down =
              let
                newRelayStates = Map.insert upstreamRelay (UpstreamRelayStatePendingRegistration portNumber) upstreamRelayStates
              in
                applyDownstreamPlan common downstream{run = run{ upstreamRelayStates = newRelayStates } }

            slotConfig slotConfiguration
              | Nothing <- run.slotConfiguration = do
                _ <- logInfo "Received slot configuration" {slotConfiguration}
                setSlotConfigurationFFI (relayKeyFromState state) slotConfiguration
                sendSlotConfigurationToPeers slotConfiguration config
                pure $ StateDownstream common downstream{run = run{ slotConfiguration = Just slotConfiguration }}
              | otherwise =
                pure state

          CastNoReply <$> processGunMessage' noop down slotConfig socket gunMsg

        Nothing ->
          pure $ CastNoReply state

    processGunMessage' :: forall a. Effect a -> Effect a -> (SlotConfiguration -> Effect a) -> WebSocket -> WsGun.GunMsg -> Effect a
    processGunMessage' noop down slotConfig socket gunMsg = do
      processResponse <- WsGun.processMessage socket gunMsg
      case processResponse of
        Left error -> do
          _ <- logInfo "Gun process error" {error}
          noop

        Right (WsGun.Internal _) ->
          noop

        Right WsGun.WebSocketUp -> do
          noop

        Right WsGun.WebSocketDown -> do
          -- todo - kick off timer?  If websocket doesn't recover, attempt to launch new relay?
          _ <- logInfo "Relay Websocket down - but which one!!" {socket}
          down

        Right (WsGun.Frame (SlotConfig slotConfiguration)) ->
          slotConfig slotConfiguration

terminate :: Foreign -> State -> Effect Unit
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

-- -----------------------------------------------------------------------------
-- Config -> Plan Transformation
-- -----------------------------------------------------------------------------
originConfigToPlan :: CommonStateData -> OriginStreamRelayConfig -> Effect OriginStreamRelayPlan
originConfigToPlan { ingestAggregator, stateServerName } config@{ egests, downstreamRelays } = do
  pure { egests: plannedEgests
       , downstreamRelays: plannedDownstreamRelays
       }
  where
    plannedEgests = egests # Map.values # map _.deliverTo # map deliverToAddressFromDeliverToEgestServer
    plannedDownstreamRelays = downstreamRelays # Map.values <#> _.deliverTo # map deliverToAddressFromDeliverToRelayServer

downstreamConfigToPlan :: CommonStateData -> DownstreamStreamRelayConfig -> Effect DownstreamStreamRelayPlan
downstreamConfigToPlan { ingestAggregator, stateServerName } config@{ egestUpstreamRelays } = do
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
    newOriginStateData = originStateData{ config = newConfig, plan = Just newPlan }
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
    newOriginStateData = originStateData{ config = newConfig, plan = Just newPlan }
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

--------------------------------------------------------------------------------
-- Log Utilities
--------------------------------------------------------------------------------
domain :: List Atom
domain = atom <$> (show Agent.StreamRelay :  "Instance" : List.nil)

logInfo :: forall a. Logger a
logInfo = domainLog Logger.info

logWarning :: forall a. Logger a
logWarning = domainLog Logger.warning

logError :: forall a. Logger a
logError = domainLog Logger.error

domainLog :: forall a. Logger {domain :: List Atom, misc :: a} -> Logger a
domainLog = Logger.doLog domain
