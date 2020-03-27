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
       , deRegisterEgest
       , deRegisterRelay
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
import Data.Either (Either(..), hush)
import Data.Foldable (find, foldl)
import Data.FoldableWithIndex (foldlWithIndex)
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Data.Newtype (un)
import Data.Traversable (traverse)
import Data.TraversableWithIndex (traverseWithIndex)
import Data.Tuple (Tuple(..))
import Data.Tuple as PursTuple
import Effect (Effect)
import Erl.Atom (Atom, atom)
import Erl.Data.List (List, fromFoldable, toUnfoldable, (:))
import Erl.Data.List as List
import Erl.Data.Map (Map)
import Erl.Data.Map as Map
import Erl.Data.Tuple as ErlTuple
import Erl.Process (Process(..), (!))
import Foreign (Foreign)
import Logger (Logger, spy)
import Logger as Logger
import Partial.Unsafe as Unsafe
import Pinto (ServerName, StartLinkResult, isRegistered)
import Pinto.Gen (CallResult(..), CastResult(..))
import Pinto.Gen as Gen
import PintoHelper (exposeState)
import Rtsv2.Agents.CachedInstanceState as CachedInstanceState
import Rtsv2.Agents.IntraPoP (IntraPoPBusMessage(..))
import Rtsv2.Agents.IntraPoP as IntraPoP
import Rtsv2.Agents.SlotTypes (SlotConfiguration)
import Rtsv2.Agents.StreamRelayTypes (CreateRelayPayload, DeRegisterEgestPayload, DeRegisterRelayPayload, DownstreamWsMessage(..), RegisterEgestPayload, RegisterRelayPayload, RelayToRelayClientWsMessage)
import Rtsv2.Agents.TransPoP as TransPoP
import Rtsv2.Names as Names
import Rtsv2.PoPDefinition as PoPDefinition
import Rtsv2.Utils (crashIfLeft)
import Shared.Agent as Agent
import Shared.Common (Url)
import Shared.Router.Endpoint (Endpoint(..), makeUrl, makeUrlAddr, makeWsUrl, makeWsUrlAddr)
import Shared.Rtsv2.JsonLd as JsonLd
import Shared.Stream (AggregatorKey(..), RelayKey(..), SlotId(..), SlotRole)
import Shared.Types (PoPName, DeliverTo, EgestServer, RelayServer(..), Server(..), ServerAddress(..), SourceRoute, extractPoP, extractAddress)
import Shared.Types.Agent.State as PublicState
import Shared.UUID (UUID)
import SpudGun (SpudResponse(..), JsonResponseError, StatusCode(..))
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
type EgestMap = Map ServerAddress (Tuple (Process DownstreamWsMessage) (DeliverTo EgestServer))
type OriginRelayMap = Map ServerAddress (Tuple (Process DownstreamWsMessage) (DeliverTo RelayServer))
type DownstreamRelayMap = Map ServerAddress (Tuple (Process DownstreamWsMessage) DownstreamRelayWithSource)

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
registerEgest :: RegisterEgestPayload -> Process DownstreamWsMessage -> Effect (Maybe SlotConfiguration)
registerEgest payload@{slotId, slotRole, deliverTo} handler =
  Gen.doCall (serverName $ payloadToRelayKey payload) doRegisterEgest

  where
    doRegisterEgest :: State -> Effect (CallResult (Maybe SlotConfiguration) State)

    doRegisterEgest state@(StateOrigin {thisServer} { config: { egests }, run: { slotConfiguration: slotConfig } }) = do
      let
        newEgests = Map.insert egestServer (Tuple handler deliverTo) egests
      CallReply slotConfig <$> applyNewEgests newEgests state

    doRegisterEgest state@(StateDownstream {thisServer} { config: { egests }, run: { slotConfiguration: slotConfig } }) = do
      let
        newEgests = Map.insert egestServer (Tuple handler deliverTo) egests
      CallReply slotConfig <$> applyNewEgests newEgests state

    egestServer = (extractAddress deliverTo.server)

deRegisterEgest :: DeRegisterEgestPayload -> Effect Unit
deRegisterEgest {slotId, slotRole, egestServerAddress} =
  Gen.doCall (serverName $ RelayKey slotId slotRole) doDeRegisterEgest
  where
    doDeRegisterEgest state@(StateOrigin _ { config: { egests } }) = do
      let
        newEgests = Map.delete egestServer egests
      CallReply unit <$> applyNewEgests newEgests state

    doDeRegisterEgest state@(StateDownstream _ { config: { egests } }) = do
      let
        newEgests = Map.delete egestServer egests
      CallReply unit <$> applyNewEgests newEgests state

    egestServer = egestServerAddress


registerRelay :: RegisterRelayPayload -> (Process DownstreamWsMessage) -> Effect (Maybe SlotConfiguration)
registerRelay payload@{slotId, slotRole, sourceRoute, deliverTo } handler =
  Gen.doCall (serverName $ payloadToRelayKey payload) doRegisterRelay

  where
    doRegisterRelay :: State -> Effect(CallResult (Maybe SlotConfiguration) State)

    doRegisterRelay (StateOrigin commonStateData@{thisServer} originStateData@{ config: config@{ downstreamRelays }, run: { slotConfiguration: slotConfig } }) = do
      -- TODO: PS: log if we've got a non-nil source when we're in origin mode
      let
        newDownstreamRelays = Map.insert (extractAddress deliverTo.server) (Tuple handler deliverTo) downstreamRelays
      CallReply slotConfig <$> applyOriginNewRelays newDownstreamRelays commonStateData originStateData

    doRegisterRelay (StateDownstream commonStateData@{thisServer} downstreamStateData@{ config: config@{ downstreamRelays }, run: { slotConfiguration: slotConfig } }) =
      case Array.uncons sourceRoute of
        Just { head, tail } -> do
          let
            downstreamRelayWithSource =
              { source: { next: head, rest: tail }
              , deliverTo
              }
            newDownstreamRelays = Map.insert (extractAddress deliverTo.server) (Tuple handler downstreamRelayWithSource) downstreamRelays
          CallReply slotConfig <$> applyDownstreamNewRelays newDownstreamRelays commonStateData downstreamStateData

        Nothing ->
          -- TODO: PS: log if we've got a nil source when we're in downstream mode
          pure $ CallReply Nothing $ StateDownstream commonStateData downstreamStateData

    relayServer = (extractAddress deliverTo.server)

deRegisterRelay :: DeRegisterRelayPayload -> Effect Unit
deRegisterRelay {slotId, slotRole, relayServerAddress } =
  Gen.doCall (serverName $ RelayKey slotId slotRole) doDeRegisterRelay

  where
    doDeRegisterRelay :: State -> Effect (CallResult Unit State)

    doDeRegisterRelay (StateOrigin commonStateData originStateData@{ config: { downstreamRelays } }) = do
      -- TODO: PS: log if we've got a non-nil source when we're in origin mode
      let
        newDownstreamRelays = Map.delete relayServerAddress downstreamRelays
      CallReply unit <$> applyOriginNewRelays newDownstreamRelays commonStateData originStateData

    doDeRegisterRelay (StateDownstream commonStateData downstreamStateData@{ config: { downstreamRelays } }) = do
      let
        newDownstreamRelays = Map.delete relayServerAddress downstreamRelays
      CallReply unit <$> applyDownstreamNewRelays newDownstreamRelays commonStateData downstreamStateData

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
--    >>= tryEnsureOriginSlotConfiguration commonStateData

  where
    maybeTryRegisterIngestAggregator runStateIn@{ ingestAggregatorState: IngestAggregatorStateDisabled } = pure runStateIn
    maybeTryRegisterIngestAggregator runStateIn@{ ingestAggregatorState: IngestAggregatorStateRegistered _ _} = pure runStateIn
    maybeTryRegisterIngestAggregator runStateIn@{ ingestAggregatorState: IngestAggregatorStatePendingRegistration portNumber } =
      let
        wsUrl = makeWsUrl ingestAggregator $ IngestAggregatorRegisteredRelayWs slotId slotRole (extractAddress thisServer) portNumber

        -- deliverTo =
        --   { server: (thisServer # un Server # Relay)
        --   , port: portNumber
        --   }

        -- -- Should this be a different type? sourceRoute is redundant when registered with an IA
        -- registerPayload :: RegisterRelayPayload
        -- registerPayload = {slotId, slotRole, deliverTo, sourceRoute: List.nil}
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
--    >>= tryEnsureDownstreamSlotConfiguration commonStateData

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

              -- todo
              -- if registerResult then
              --   pure $ UpstreamRelayStateRegistered portNumber relayAddress
              -- else
              --   pure $ UpstreamRelayStatePendingRegistration portNumber

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

-- tryEnsureOriginSlotConfiguration :: CommonStateData -> OriginStreamRelayRunState -> Effect OriginStreamRelayRunState

-- tryEnsureOriginSlotConfiguration _commonStateData runStateIn@{ slotConfiguration: Just _ } =
--   pure runStateIn

-- tryEnsureOriginSlotConfiguration { relayKey: relayKey@(RelayKey slotId slotRole), ingestAggregator } runStateIn =
--   do
--     response <- fetchIngestAggregatorSlotConfiguration ingestAggregator slotId slotRole

--     case hush response of
--       Nothing ->
--         pure runStateIn

--       Just receivedSlotConfiguration ->
--         do
--           setSlotConfigurationFFI relayKey receivedSlotConfiguration
--           pure runStateIn{ slotConfiguration = Just receivedSlotConfiguration }

-- tryEnsureDownstreamSlotConfiguration :: CommonStateData -> DownstreamStreamRelayRunState -> Effect DownstreamStreamRelayRunState

-- tryEnsureDownstreamSlotConfiguration _commonStateData runStateIn@{ slotConfiguration: Just _ } =
--   pure runStateIn

-- tryEnsureDownstreamSlotConfiguration { relayKey: relayKey@(RelayKey slotId slotRole) } runStateIn@{ upstreamRelayStates } =
--   do
--     result <- tryEnsureSlotConfigurationFromRelays' (Map.values upstreamRelayStates)

--     case result of
--       Nothing ->
--         pure runStateIn

--       Just receivedSlotConfiguration ->
--         do
-- -- todo - whole clause not needed          setSlotConfigurationFFI relayKey receivedSlotConfiguration
--           pure runStateIn{ slotConfiguration = Just receivedSlotConfiguration }

--   where
--     tryEnsureSlotConfigurationFromRelays' upstreamRelayStates =
--       case List.uncons upstreamRelayStates of
--         Nothing ->
--           pure Nothing

--         Just { head, tail } ->
--           case head of
--             UpstreamRelayStateRegistered portNumber serverAddress _webSocket ->
--               do
--                 -- todo - won't need this...
--                 response <- fetchStreamRelaySlotConfiguration serverAddress slotId slotRole

--                 case hush response of
--                   Just receivedSlotConfiguration ->
--                     pure $ Just receivedSlotConfiguration

--                   _ ->
--                     tryEnsureSlotConfigurationFromRelays' tail

--             relayInOtherState ->
--               tryEnsureSlotConfigurationFromRelays' tail

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
          , relaysServed : JsonLd.downstreamRelayLocationNode slotId slotRole <$> PursTuple.snd <$> Map.values originStateData.config.downstreamRelays
          }
    mkStatus (StateDownstream {relayKey: RelayKey slotId slotRole, thisServer} downstreamStateData) =
      JsonLd.streamRelayStateNode slotId publicState thisServer
      where
        publicState =
          { role : slotRole
          , egestsServed : JsonLd.egestServedLocationNode slotId slotRole <$> Map.keys downstreamStateData.config.egests
          , relaysServed : JsonLd.downstreamRelayLocationNode slotId slotRole <$> _.deliverTo <$> PursTuple.snd <$> Map.values downstreamStateData.config.downstreamRelays
          }

-- slotConfiguration :: RelayKey -> Effect (Maybe SlotConfiguration)
-- slotConfiguration relayKey =
--   getSlotConfigurationFFI relayKey

-- -----------------------------------------------------------------------------
-- gen Server Implementation
-- -----------------------------------------------------------------------------
data Msg = Init
         | IntraPoPBus IntraPoP.IntraPoPBusMessage
         | Gun WsGun.GunMsg

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
  deRegisterFromPeers cachedState thisServer
  IntraPoP.announceLocalRelayStopped relayKey
  where
    deRegisterFromPeers Nothing thisServer =
      pure unit

    deRegisterFromPeers (Just (CachedOrigin commonState _ maybeRunState)) thisServer =
      deRegisterOrigin commonState maybeRunState thisServer

    deRegisterFromPeers (Just (CachedDownstream _ maybeRunState)) thisServer =
      deRegisterUpstreams maybeRunState thisServer

    deRegisterOrigin { ingestAggregator} (Just {ingestAggregatorState: IngestAggregatorStateRegistered _ _}) thisServer = do
       -- todo
      -- let
      --   deleteUrl = makeUrl ingestAggregator $ IngestAggregatorRegisteredRelayE slotId slotRole (extractAddress thisServer)
      -- void <$> crashIfLeft =<< SpudGun.delete deleteUrl {}
      pure unit

    deRegisterOrigin _ _ _ =
      pure unit

    deRegisterUpstreams Nothing _ =
      pure unit

    deRegisterUpstreams (Just {upstreamRelayStates}) thisServer = do
-- todo      _ <- traverse (deRegisterUpstream thisServer) (Map.values upstreamRelayStates)
      pure unit

    -- deRegisterUpstream thisServer (UpstreamRelayStatePendingDeregistration _portNumber relayAddress) = do
    --   let
    --     deleteUrl = makeUrlAddr relayAddress $ RelayRegisteredRelayE slotId slotRole (extractAddress thisServer)
    --   void <$> crashIfLeft =<< SpudGun.delete deleteUrl {}
    --   pure unit

    -- deRegisterUpstream _ _ =
    --   pure unit

init :: RelayKey -> CreateRelayPayload -> StateServerName -> Effect State
init relayKey payload@{slotId, slotRole, aggregator} stateServerName =
  do
    Gen.registerExternalMapping (serverName relayKey) (\m -> Gun <$> (WsGun.messageMapper m))
    thisServer <- PoPDefinition.getThisServer
    egestSourceRoutes <- TransPoP.routesTo (extractPoP aggregator)


    IntraPoP.announceLocalRelayIsAvailable relayKey
    _ <- Bus.subscribe (serverName relayKey) IntraPoP.bus IntraPoPBus
    Gen.registerTerminate (serverName relayKey) terminate

    let
      commonStateData =
        { relayKey
        , thisServer
        , ingestAggregator: aggregator
        , stateServerName
        }

    if egestSourceRoutes == List.nil then
      do
        workflowHandle <- startOriginWorkflowFFI (un SlotId slotId)
        config <- getCachedOriginConfig
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

handleInfo :: Msg -> State -> Effect (CastResult State)
handleInfo msg state =
  case msg of
    Init ->
      pure $ CastNoReply state

    IntraPoPBus (IngestAggregatorExited (AggregatorKey slotId slotRole) serverAddress)
     -- TODO - PRIMARY BACKUP
      | slotId == ourSlotId -> pure $ CastStop state
      | otherwise -> pure $ CastNoReply state

    IntraPoPBus (VmReset _ _ _) ->
      pure $ CastNoReply state

    Gun inMsg ->
      processGunMessage state inMsg

  where
    (RelayKey ourSlotId _) = relayKeyFromState state

    findUpstreamSocket :: WsGun.GunMsg -> Map UpstreamRelay UpstreamRelayState -> Maybe WebSocket
    findUpstreamSocket gunMsg map =
      List.head $ List.filter (WsGun.isSocketForMessage gunMsg) $ List.catMaybes $ getSocket <$> Map.values map
      where
        getSocket (UpstreamRelayStatePendingRegistration _portNumber) = Nothing
        getSocket (UpstreamRelayStateRegistered _portNumber _serverAddress socket) = Just socket
        getSocket (UpstreamRelayStatePendingDeregistration _portNumber _serverAddress socket) = Just socket
        getSocket (UpstreamRelayStateDeregistered _portNumber) = Nothing

    findAggregatorSocket :: WsGun.GunMsg -> IngestAggregatorState -> Maybe WebSocket
    findAggregatorSocket gunMsg IngestAggregatorStateDisabled = Nothing
    findAggregatorSocket gunMsg (IngestAggregatorStatePendingRegistration _) = Nothing
    findAggregatorSocket gunMsg (IngestAggregatorStateRegistered _ socket) = Just socket

    sendSlotConfiguration :: SlotConfiguration -> (Process DownstreamWsMessage) -> Effect Unit
    sendSlotConfiguration slotConfiguration process =
      process ! SlotConfig slotConfiguration

    maybeSendSlotConfiguration :: forall r egestKey egestServer relayKey relayServer. Maybe SlotConfiguration -> Maybe SlotConfiguration -> {egests :: Map egestKey (Tuple (Process DownstreamWsMessage) egestServer),
                                                                                                                                             downstreamRelays :: Map relayKey (Tuple (Process DownstreamWsMessage) relayServer) | r} -> Effect Unit
    maybeSendSlotConfiguration Nothing (Just newSlotConfiguration) {egests, downstreamRelays} = do
      let
        egestPids = Map.values egests <#> PursTuple.fst
        relayPids = Map.values downstreamRelays <#> PursTuple.fst
      _ <- traverse (sendSlotConfiguration newSlotConfiguration) (egestPids <> relayPids)
      pure unit
    maybeSendSlotConfiguration _oldRun _newRun _config =
      pure unit

    processGunMessage (StateOrigin common origin@{config, run: run@{ingestAggregatorState}}) gunMsg = do
      case findAggregatorSocket gunMsg ingestAggregatorState of
        Just socket -> do
          run2 <- processGunMessage' run socket gunMsg
          maybeSendSlotConfiguration run.slotConfiguration run2.slotConfiguration config
          pure $ CastNoReply $ StateOrigin common origin{run = run2}
        Nothing ->
          pure $ CastNoReply state

    processGunMessage (StateDownstream common downstream@{config, run: run@{upstreamRelayStates}}) gunMsg =
      case findUpstreamSocket gunMsg upstreamRelayStates of
        Just socket -> do
          run2 <- processGunMessage' run socket gunMsg
          maybeSendSlotConfiguration run.slotConfiguration run2.slotConfiguration config
          pure $ CastNoReply $ StateDownstream common downstream{run = run2}
        Nothing ->
          pure $ CastNoReply state

    processGunMessage' :: forall r. { slotConfiguration :: Maybe SlotConfiguration | r} ->
                                    WebSocket ->
                                    WsGun.GunMsg ->
                                    Effect { slotConfiguration :: Maybe SlotConfiguration | r}
    processGunMessage' run socket gunMsg = do
      processResponse <- WsGun.processMessage socket gunMsg
      case processResponse of
        Left error -> do
          _ <- logInfo "Gun process error" {error}
          pure $ run

        Right (WsGun.Internal _) ->
          pure $ run

        Right WsGun.WebSocketUp -> do
          pure $ run

        Right WsGun.WebSocketDown -> do
          -- todo - kick off timer?  If websocket doesn't recover, attempt to launch new relay?
          pure $ run

        Right (WsGun.Frame (SlotConfig slotConfiguration))
          | Nothing <- run.slotConfiguration -> do
            _ <- logInfo "Received slot configuration" {slotConfiguration}
            setSlotConfigurationFFI (relayKeyFromState state) slotConfiguration
            pure run{ slotConfiguration = Just slotConfiguration }

          | otherwise ->
            pure run

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
    plannedEgests = egests # Map.values # map PursTuple.snd # map deliverToAddressFromDeliverToEgestServer
    plannedDownstreamRelays = downstreamRelays # Map.values <#> PursTuple.snd # map deliverToAddressFromDeliverToRelayServer

downstreamConfigToPlan :: CommonStateData -> DownstreamStreamRelayConfig -> Effect DownstreamStreamRelayPlan
downstreamConfigToPlan { ingestAggregator, stateServerName } config@{ egestUpstreamRelays } = do
  pure { egests: plannedEgests
       , downstreamRelays: plannedDownstreamRelays
       , upstreamRelaySources
       }
  where
    egestList = config.egests # Map.values

    plannedEgests = egestList <#> PursTuple.snd <#> deliverToAddressFromDeliverToEgestServer

    plannedDownstreamRelays = downstreamRelayList <#> PursTuple.snd # map (_.deliverTo) # map deliverToAddressFromDeliverToRelayServer

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
        <#> PursTuple.snd
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


emptyOriginConfig :: OriginStreamRelayConfig
emptyOriginConfig =
  { egests: Map.empty, downstreamRelays: Map.empty }

emptyDownstreamConfig :: List UpstreamRelay -> DownstreamStreamRelayConfig
emptyDownstreamConfig egestUpstreamRelays =
  { egestUpstreamRelays, egests: Map.empty, downstreamRelays: Map.empty }

mkUpstreamRelay :: SourceRoute -> UpstreamRelay
mkUpstreamRelay route =
  let
    { head: next, tail: rest } = Unsafe.unsafePartial $ Maybe.fromJust $ Array.uncons route
  in
    { next, rest }

-- fetchIngestAggregatorSlotConfiguration :: Server -> SlotId -> SlotRole -> Effect (Either JsonResponseError SlotConfiguration)
-- fetchIngestAggregatorSlotConfiguration aggregator slotId slotRole =
--   fetchSlotConfiguration $ makeUrl aggregator $ IngestAggregatorSlotConfigurationE slotId slotRole

-- fetchStreamRelaySlotConfiguration :: ServerAddress -> SlotId -> SlotRole -> Effect (Either JsonResponseError SlotConfiguration)
-- fetchStreamRelaySlotConfiguration relay slotId slotRole =
--   fetchSlotConfiguration $ makeUrlAddr relay $ RelaySlotConfigurationE slotId slotRole

-- fetchSlotConfiguration :: Url -> Effect (Either JsonResponseError SlotConfiguration)
-- fetchSlotConfiguration url =
--   SpudGun.getJson url <#> SpudGun.bodyToJSON

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
