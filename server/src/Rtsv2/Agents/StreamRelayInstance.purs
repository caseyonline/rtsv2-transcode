module Rtsv2.Agents.StreamRelayInstance
  ( startLink
  , isAvailable
  , registerEgest
  , registerRelay
  , init
  , status
  , slotConfiguration
  , State
  ) where

import Prelude

import Bus as Bus
import Data.Either (fromRight)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.Set (Set)
import Data.Set as Set
import Effect (Effect)
import Erl.Atom (Atom, atom)
import Erl.Data.List (List, nil, uncons, (:))
import Erl.Data.Map (Map)
import Erl.Data.Map as Map
import Erl.Utils (Url)
import Logger (Logger, spy)
import Logger as Logger
import Partial.Unsafe (unsafePartial)
import Pinto (ServerName, StartLinkResult, isRegistered)
import Pinto.Gen (CallResult(..), CastResult(..))
import Pinto.Gen as Gen
import PintoHelper (exposeState)
import Rtsv2.Agents.IntraPoP (IntraPoPBusMessage(..))
import Rtsv2.Agents.IntraPoP as IntraPoP
import Rtsv2.Agents.SlotTypes (SlotConfiguration)
import Rtsv2.Agents.StreamRelayTypes (CreateRelayPayload, RegisterEgestPayload, SourceRoute, RegisterRelayPayload, DeliverTo)
import Rtsv2.Agents.TransPoP as TransPoP
import Rtsv2.Names as Names
import Rtsv2.PoPDefinition as PoPDefinition
import Rtsv2.Router.Endpoint (Endpoint(..), makeUrl, makeUrlWithPath)
import Shared.Agent as Agent
import Shared.Stream (AggregatorKey(..), RelayKey(..), StreamId(..), StreamRole(..))
import Shared.Types (EgestServer, RelayServer(..), Server, extractPoP, extractAddress)
import Shared.Types.Agent.State as PublicState
import SpudGun as SpudGun

type Port = Int
type Host = String

foreign import data Handle :: Type
foreign import startWorkflowFFI :: String -> Effect Handle
foreign import setSlotConfigurationFFI :: RelayKey -> SlotConfiguration -> Effect Unit
foreign import getSlotConfigurationFFI :: RelayKey -> Effect (Maybe SlotConfiguration)
foreign import addIngestAggregatorSourceFFI :: Handle -> Effect Port
foreign import addEgestSinkFFI :: Host -> Port -> Handle -> Effect Unit

data Msg = IntraPoPBus IntraPoP.IntraPoPBusMessage

data State =
  State
  { relayKey :: RelayKey
  , aggregator :: Server
  , thisServer :: Server
  , relaysServed :: Map (DeliverTo RelayServer) (Set SourceRoute)
  , egestsServed :: Set (DeliverTo EgestServer)
  , egestSourceRoutes :: Maybe (List SourceRoute)
  , workflowHandle :: Handle
  }


payloadToRelayKey :: forall r.
  { streamId :: StreamId
  , streamRole :: StreamRole
  | r
  }
  -> RelayKey
payloadToRelayKey payload = RelayKey payload.streamId payload.streamRole

serverName :: RelayKey -> ServerName State Msg
serverName = Names.streamRelayInstanceName

startLink :: CreateRelayPayload -> Effect StartLinkResult
startLink payload =
  let
    relayKey = payloadToRelayKey payload
  in
  Gen.startLink (serverName relayKey) (init payload) handleInfo

isAvailable :: RelayKey -> Effect Boolean
isAvailable relayKey = isRegistered (serverName relayKey)

status :: RelayKey -> Effect (PublicState.StreamRelay List)
status =
  exposeState mkStatus <<< serverName
  where
    mkStatus (State state) =
       { egestsServed : extractAddress <$> _.server <$> Set.toUnfoldable state.egestsServed
       , relaysServed : extractAddress <$> _.server <$> Map.keys state.relaysServed
       }

slotConfiguration :: RelayKey -> Effect (Maybe SlotConfiguration)
slotConfiguration relayKey =
  getSlotConfigurationFFI relayKey

init :: CreateRelayPayload -> Effect State
init payload@{streamId, streamRole} = do
  let
    relayKey = RelayKey streamId streamRole
  logInfo "StreamRelay starting" {payload}
  thisServer <- PoPDefinition.getThisServer
  workflowHandle <- startWorkflowFFI (unwrap streamId)
  _ <- Bus.subscribe (serverName relayKey) IntraPoP.bus IntraPoPBus

  IntraPoP.announceLocalRelayIsAvailable relayKey
  -- TODO - linger timeout / exit if idle
  pure $ State { relayKey: relayKey
               , aggregator : payload.aggregator
               , thisServer
               , relaysServed : Map.empty
               , egestsServed : mempty
               , egestSourceRoutes : Nothing
               , workflowHandle
               }

handleInfo :: Msg -> State -> Effect (CastResult State)
handleInfo msg state@(State {relayKey: RelayKey ourStreamId _}) =
  case msg of
    IntraPoPBus (IngestAggregatorExited (AggregatorKey streamId streamRole) serverAddress)
     -- TODO - PRIMARY BACKUP
      | streamId == ourStreamId -> doStop state
      | otherwise -> pure $ CastNoReply state

doStop :: State -> Effect (CastResult State)
doStop state@(State {relayKey}) = do
  logInfo "Stream Relay stopping" {relayKey}
  IntraPoP.announceLocalRelayStopped relayKey
  pure $ CastStop state


-- TODO: PS: change to use Nick's new routing when available
registerEgest :: RegisterEgestPayload -> Effect Unit
registerEgest payload@{streamId, streamRole} = Gen.doCall (serverName $ payloadToRelayKey payload) doRegisterEgest
  where
    doRegisterEgest :: State -> Effect (CallResult Unit State)

    doRegisterEgest (State state@{egestSourceRoutes: Nothing, aggregator, workflowHandle, thisServer, relayKey}) =
      do
        egestSourceRoutes <- TransPoP.routesTo (extractPoP aggregator)

        if egestSourceRoutes == nil then
          registerWithIngestAggregator state

        else
          pure unit

        doRegisterEgest (State state{egestSourceRoutes = Just (spy "Egest Source Routes" egestSourceRoutes)})

    doRegisterEgest (State state@{egestsServed, workflowHandle}) =
      do
        logInfo "Register egest " {payload}
        addEgestSinkFFI (payload.deliverTo.server # unwrap # _.address # unwrap) payload.deliverTo.port workflowHandle
        let
          newState = state{ egestsServed = Set.insert payload.deliverTo egestsServed}
        -- newState <- maybeStartRelaysForEgest state{ egestsServed = Set.insert payload.deliverTo egestsServed}
        pure $ CallReply unit $ spy "registerEgest" (State newState)

    registerWithIngestAggregator {aggregator, workflowHandle, thisServer, relayKey} =
      do

        slotConfigurationFromIA <- fetchIngestAggregatorSlotConfiguration aggregator streamId streamRole
        setSlotConfigurationFFI relayKey (spy "Config Response" slotConfigurationFromIA)

        -- register a new source with the ingest aggregator
        receivePort <- addIngestAggregatorSourceFFI workflowHandle

        let
          registerURL = makeUrl aggregator IngestAggregatorRegisterRelayE

          deliverTo =
            { server: (thisServer # unwrap # Relay)
            , port: receivePort
            }

          -- TODO: sourceRoute shouldn't be needed?
          registerPayload :: RegisterRelayPayload
          registerPayload = {streamId, streamRole, deliverTo, sourceRoute: nil}

        logInfo "Registering with aggregator" {registerPayload}
        void $ SpudGun.postJson registerURL registerPayload

        pure unit

fetchIngestAggregatorSlotConfiguration :: Server -> StreamId -> StreamRole -> Effect SlotConfiguration
fetchIngestAggregatorSlotConfiguration aggregator (StreamId streamId) streamRole =
  fetchSlotConfiguration configURL

  where
    streamRoleString =
      case streamRole of
        Primary ->
          "primary"
        Backup ->
          "backup"

    configURL = makeUrlWithPath aggregator $ "/api/agents/ingestAggregator/" <> streamId <> "/" <> streamRoleString <> "/slot"

fetchSlotConfiguration :: Url -> Effect SlotConfiguration
fetchSlotConfiguration url =
  do
    response <- SpudGun.getJson url

    pure $ unsafePartial $ fromRight $ (SpudGun.bodyToJSON response)

    -- maybeStartRelaysForEgest :: State -> Effect State
    -- maybeStartRelaysForEgest state@{egestSourceRoutes: Just _} = pure state
    -- maybeStartRelaysForEgest state@{relayKey, aggregatorPoP, thisServer} = do
    --   relayRoutes <- TransPoP.routesTo aggregatorPoP
    --   traverse_ (registerWithRelayProxy relayKey aggregatorPoP) relayRoutes
    --   pure state{egestSourceRoutes = Just (spy "relayRoutes" relayRoutes)}




registerRelay :: RegisterRelayPayload -> Effect Unit
registerRelay payload = Gen.doCast (serverName $ payloadToRelayKey payload)
  \(State state@{relayKey, relaysServed, egestSourceRoutes, aggregator}) -> do
    logInfo "Register relay chain " {payload}
    -- Do we already have this registration
    let
      routesViaThisServer = fromMaybe Set.empty $ Map.lookup payload.deliverTo relaysServed
    if Set.member payload.sourceRoute routesViaThisServer
    then do
      logWarning "Duplicate Relay Registration" {payload}
      pure $ Gen.CastNoReply $ State state
    else do
      let
        newRelaysServed = Map.insert payload.deliverTo (Set.insert payload.sourceRoute routesViaThisServer) relaysServed
      registerWithRelayProxy relayKey aggregator payload.sourceRoute
      pure $ Gen.CastNoReply $ State (state {relaysServed = newRelaysServed})



registerWithRelayProxy :: RelayKey -> Server -> SourceRoute -> Effect Unit
registerWithRelayProxy relayKey@(RelayKey streamId streamRole) aggregator sourceRoute = do
  case uncons sourceRoute of
    Just {head: nextPoP, tail: remainingRoute} -> do
      -- -- TODO okAlreadyStarted
      -- _ <- DownstreamProxy.startLink {streamId, streamRole, proxyFor: nextPoP, aggregator}
      -- -- TODO ok
      -- DownstreamProxy.addRelayRoute relayKey  nextPoP remainingRoute
      pure unit
    Nothing ->
      -- This relay request is for content aggregated in this popdefinition
      -- TODO - media stuff
      pure unit


--------------------------------------------------------------------------------
-- Log helpers
--------------------------------------------------------------------------------
domains :: List Atom
domains = atom <$> (show Agent.StreamRelay :  "Instance" : nil)

logInfo :: forall a. Logger a
logInfo = domainLog Logger.info

logWarning :: forall a. Logger a
logWarning = domainLog Logger.warning

domainLog :: forall a. Logger {domain :: List Atom, misc :: a} -> Logger a
domainLog = Logger.doLog domains
