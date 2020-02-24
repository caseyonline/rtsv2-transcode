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
import Control.Bind (bindFlipped)
import Data.Either (Either(..), fromRight, note, either)
import Data.Filterable (filterMap)
import Data.Foldable (traverse_, find)
import Data.Traversable (sequence, traverse)
import Data.Maybe (Maybe(..), fromMaybe, fromJust)
import Data.Newtype (unwrap)
import Data.Set (Set)
import Data.Set as Set
import Effect (Effect)
import Erl.Atom (Atom, atom)
import Erl.Data.List (List, nil, uncons, (:))
import Erl.Data.List as List
import Erl.Data.Map (Map)
import Erl.Data.Map as Map
import Erl.Data.Tuple (fst, snd)
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
import Rtsv2.Agents.TransPoP (PoPRoutes)
import Rtsv2.Agents.TransPoP as TransPoP
import Rtsv2.Names as Names
import Rtsv2.PoPDefinition as PoPDefinition
import Rtsv2.Router.Endpoint (Endpoint(..), makeUrlAddr, makeUrlAddrWithPath, makeUrl, makeUrlWithPath)
import Shared.Agent as Agent
import Shared.Stream (AggregatorKey(..), RelayKey(..), SlotId(..), SlotRole(..))
import Shared.Types (PoPName, EgestServer, RelayServer(..), Server, ServerAddress(..), extractPoP, extractAddress)
import Shared.Types.Agent.State as PublicState
import SpudGun (SpudResponse(..), SpudError, JsonResponseError)
import SpudGun as SpudGun

type Port = Int
type Host = String

foreign import data Handle :: Type
foreign import startWorkflowFFI :: Int -> Effect Handle
foreign import setSlotConfigurationFFI :: RelayKey -> SlotConfiguration -> Effect Unit
foreign import getSlotConfigurationFFI :: RelayKey -> Effect (Maybe SlotConfiguration)
foreign import ensureIngestAggregatorSourceFFI :: Handle -> Effect Port
foreign import ensureStreamRelaySourceFFI :: SourceRoute -> Handle -> Effect Port
foreign import addEgestSinkFFI :: Host -> Port -> Handle -> Effect Unit

data Msg = IntraPoPBus IntraPoP.IntraPoPBusMessage

data State = State StateData

type StateData =
  { relayKey :: RelayKey
  , aggregator :: Server
  , thisServer :: Server
  , relaysServed :: Map (DeliverTo RelayServer) (Set SourceRoute)
  , egestsServed :: Set (DeliverTo EgestServer)
  , egestSource :: Maybe EgestSource
  , workflowHandle :: Handle
  }


payloadToRelayKey :: forall r.
  { slotId :: SlotId
  , streamRole :: SlotRole
  | r
  }
  -> RelayKey
payloadToRelayKey payload = RelayKey payload.slotId payload.streamRole

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
init payload@{slotId, streamRole} = do
  let
    relayKey = RelayKey slotId streamRole
  logInfo "StreamRelay starting" {payload}
  thisServer <- PoPDefinition.getThisServer
  workflowHandle <- startWorkflowFFI (unwrap slotId)
  _ <- Bus.subscribe (serverName relayKey) IntraPoP.bus IntraPoPBus

  IntraPoP.announceLocalRelayIsAvailable relayKey
  -- TODO - linger timeout / exit if idle
  pure $ State { relayKey: relayKey
               , aggregator: payload.aggregator
               , thisServer
               , relaysServed: Map.empty
               , egestsServed: mempty
               , egestSource: Nothing
               , workflowHandle
               }

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


-- TODO: PS: change to use Nick's new routing when available
registerEgest :: RegisterEgestPayload -> Effect Unit
registerEgest payload@{slotId, streamRole} =
  Gen.doCall (serverName $ payloadToRelayKey payload) doRegisterEgest

  where
    doRegisterEgest :: State -> Effect (CallResult Unit State)

    doRegisterEgest (State state@{egestSource: Nothing, aggregator, workflowHandle, thisServer, relayKey}) =
      do
        egestSourceRoutes <- TransPoP.routesTo (extractPoP aggregator)

        egestSource <- healthCheckEgestSource state $ mkEgestSource (spy "Egest Source Routes" egestSourceRoutes)

        doRegisterEgest (State state{egestSource = Just egestSource})

    doRegisterEgest (State state@{egestsServed, workflowHandle}) =
      do
        logInfo "Register egest " {payload}
        addEgestSinkFFI (payload.deliverTo.server # unwrap # _.address # unwrap) payload.deliverTo.port workflowHandle

        let
          newState = state{ egestsServed = Set.insert payload.deliverTo egestsServed }

        pure $ CallReply unit $ spy "registerEgest" (State newState)

registerRelay :: RegisterRelayPayload -> Effect Unit
registerRelay payload@{ deliverTo
                      , sourceRoute
                      } =

  Gen.doCast (serverName $ payloadToRelayKey payload) doRegisterRelay

  where
    doRegisterRelay state =
      do
        ensureSourceRoute
        ensureDeliverToRelay
        pure $ Gen.CastNoReply $ state

    ensureSourceRoute =
      pure unit

    ensureDeliverToRelay =
      pure unit

-- registerRelay :: RegisterRelayPayload -> Effect Unit
-- registerRelay payload = Gen.doCast (serverName $ payloadToRelayKey payload)
--   \(State state@{relayKey, relaysServed, aggregator}) -> do
--     logInfo "Register relay chain " {payload}
--
--     -- Do we already have this registration
--     let
--       routesViaThisServer = fromMaybe Set.empty $ Map.lookup payload.deliverTo relaysServed
--
--     if Set.member payload.sourceRoute routesViaThisServer then
--       do
--         logWarning "Duplicate Relay Registration" {payload}
--         pure $ Gen.CastNoReply $ State state
--     else
--       do
--         let
--           newRelaysServed = Map.insert payload.deliverTo (Set.insert payload.sourceRoute routesViaThisServer) relaysServed
--         registerWithRelayProxy relayKey aggregator payload.sourceRoute
--         pure $ Gen.CastNoReply $ State (state {relaysServed = newRelaysServed})
--
--
-- registerWithRelayProxy :: RelayKey -> Server -> SourceRoute -> Effect Unit
-- registerWithRelayProxy relayKey@(RelayKey slotId streamRole) aggregator sourceRoute = do
--   case uncons sourceRoute of
--     Just {head: nextPoP, tail: remainingRoute} -> do
--       -- -- TODO okAlreadyStarted
--       -- _ <- DownstreamProxy.startLink {slotId, streamRole, proxyFor: nextPoP, aggregator}
--       -- -- TODO ok
--       -- DownstreamProxy.addRelayRoute relayKey  nextPoP remainingRoute
--       pure unit
--     Nothing ->
--       -- This relay request is for content aggregated in this popdefinition
--       -- TODO - media stuff
--       pure unit

--------------------------------------------------------------------------------
-- Egest Source
--------------------------------------------------------------------------------
data EgestSource
  = IngestAggregatorSource IngestAggregatorSourceState
  | RedundantStreamRelaysSource RedundantStreamRelaysSourceData

data IngestAggregatorSourceState
  = IngestAggregatorSourceStatePending
  | IngestAggregatorSourceStateRunning

type RedundantStreamRelaysSourceData =
  { sources :: List StreamRelaySource
  }

data StreamRelaySource
  = StreamRelaySourcePendingRelay StreamRelaySourceRouteInfo
  | StreamRelaySourceAllocatedRelay StreamRelaySourceAllocatedRelayData

type StreamRelaySourceRouteInfo =
  { nextPoP :: PoPName
  , remainingRoute :: List PoPName
  }

type StreamRelaySourceAllocatedRelayData =
  { routeInfo :: StreamRelaySourceRouteInfo
  , chosenRelay :: ServerAddress
  , slotConfiguration :: Maybe SlotConfiguration
  }

mkEgestSource :: PoPRoutes -> EgestSource
mkEgestSource routes =
  if routes == nil then
    IngestAggregatorSource IngestAggregatorSourceStatePending
  else
    RedundantStreamRelaysSource { sources: map mkStreamRelaySource routes }

  where
    mkStreamRelaySource route =
      let
        { head: nextPoP, tail: remainingRoute } = unsafePartial $ fromJust $ uncons route
      in
        StreamRelaySourcePendingRelay
        { nextPoP
        , remainingRoute
        }


healthCheckEgestSource :: StateData -> EgestSource -> Effect EgestSource
healthCheckEgestSource state source =
  case source of
    IngestAggregatorSource IngestAggregatorSourceStateRunning ->
      pure source

    IngestAggregatorSource IngestAggregatorSourceStatePending ->
      healthCheckIngestAggregator state
        <#> IngestAggregatorSource

    RedundantStreamRelaysSource { sources } ->
      traverse (healthCheckStreamRelay state) sources
        <#> \newSources -> RedundantStreamRelaysSource { sources: newSources }

healthCheckIngestAggregator :: StateData -> Effect IngestAggregatorSourceState
healthCheckIngestAggregator { relayKey: relayKey@(RelayKey slotId slotRole)
                            , aggregator
                            , thisServer
                            , workflowHandle
                            } =
  do

    slotConfigurationFromIA <- fetchIngestAggregatorSlotConfiguration aggregator slotId slotRole
    setSlotConfigurationFFI relayKey (spy "Config Response" slotConfigurationFromIA)

    -- register a new source with the ingest aggregator
    receivePort <- ensureIngestAggregatorSourceFFI workflowHandle

    let
      registerURL = makeUrl aggregator IngestAggregatorRegisterRelayE

      deliverTo =
        { server: (thisServer # unwrap # Relay)
        , port: receivePort
        }

      -- TODO: sourceRoute shouldn't be needed?
      registerPayload :: RegisterRelayPayload
      registerPayload = {slotId, streamRole: slotRole, deliverTo, sourceRoute: nil}

    logInfo "Registering with aggregator" {registerPayload}
    void $ SpudGun.postJson registerURL registerPayload

    pure IngestAggregatorSourceStateRunning

data ConnectStreamRelayFailure
  = NoRandomServerFound PoPName
  | EnsureRequestFailed SpudError
  | EnsureRequestServedByHeaderMissing
  | RegisterRequestFailed SpudError
  | EnsureSlotConfigurationFailed JsonResponseError

healthCheckStreamRelay :: StateData -> StreamRelaySource -> Effect StreamRelaySource
healthCheckStreamRelay
    { relayKey: relayKey@(RelayKey slotId slotRole)
    , aggregator
    , thisServer
    , workflowHandle
    }
    source
  =
    -- TODO: PS: if the relay is chosen, should that always be used? even if
    -- subsequent steps fail?
    ensureRandomRelayChosen source
      >>= chainEither ensureRelay
      >>= chainEither connectRelay
      >>= chainEither ensureSlotConfiguration
      <#> map StreamRelaySourceAllocatedRelay
      >>= handleFailure

  where
    ensureRandomRelayChosen :: StreamRelaySource -> Effect (Either ConnectStreamRelayFailure StreamRelaySourceAllocatedRelayData)
    ensureRandomRelayChosen (StreamRelaySourceAllocatedRelay allocatedRelayData) = pure $ Right $ allocatedRelayData
    ensureRandomRelayChosen (StreamRelaySourcePendingRelay routeInfo@{ nextPoP }) =
      PoPDefinition.getRandomServerInPoP nextPoP
      <#> note (NoRandomServerFound nextPoP)
      <#> map (\serverAddress -> { routeInfo, chosenRelay: serverAddress, slotConfiguration: Nothing })

    ensureRelay :: StreamRelaySourceAllocatedRelayData -> Effect (Either ConnectStreamRelayFailure StreamRelaySourceAllocatedRelayData)
    ensureRelay sourceIn@{ chosenRelay: chosenRelay } =
      let
        payload = {slotId, streamRole: slotRole, aggregator} :: CreateRelayPayload
        url = makeUrlAddr chosenRelay RelayEnsureStartedE
      in
        -- TODO: PS: check response code
        SpudGun.postJsonFollow url payload
          <#> mapLeft EnsureRequestFailed
          <#> bindFlipped (\spudResponse -> extractServedByHeader spudResponse # note EnsureRequestServedByHeaderMissing)
          <#> map (\serverAddress -> sourceIn{ chosenRelay = serverAddress })

    connectRelay :: StreamRelaySourceAllocatedRelayData -> Effect (Either ConnectStreamRelayFailure StreamRelaySourceAllocatedRelayData)
    connectRelay streamIn@{ routeInfo: { nextPoP, remainingRoute }, chosenRelay }  =
      do
        receivePort <- ensureStreamRelaySourceFFI (nextPoP : remainingRoute) workflowHandle

        let
          deliverTo =
            { server: (thisServer # unwrap # Relay)
            , port: receivePort
            }

          payload =
            { slotId
            , streamRole: slotRole
            , deliverTo
            , sourceRoute: remainingRoute
            }

          url = makeUrlAddr chosenRelay RelayRegisterRelayE

        _ <- (logInfo "Registering Route" payload)

        -- Don't use follow here are we should be talking to the
        -- correct server directly
        --
        -- TODO: PS: check response code
        SpudGun.postJson url payload
          <#> mapLeft RegisterRequestFailed
          <#> map (const $ streamIn)

    ensureSlotConfiguration :: StreamRelaySourceAllocatedRelayData -> Effect (Either ConnectStreamRelayFailure StreamRelaySourceAllocatedRelayData)
    ensureSlotConfiguration streamIn@{ chosenRelay } =
      let
        slotRoleString =
          case slotRole of
            Primary ->
              "primary"
            Backup ->
              "backup"

        slotIdString = show $ unwrap $ slotId

        url = spy "ensureSlotConfiguration url" (makeUrlAddrWithPath chosenRelay $ "/api/agents/relay/" <> slotIdString <> "/" <> slotRoleString <> "/slot")
      in
        -- TODO: PS: check response code
        SpudGun.getJson url
          <#> SpudGun.bodyToJSON
          <#> mapLeft EnsureSlotConfigurationFailed
          -- TODO: PS some other thing should be responsible for this...
          >>= (\result ->
                case result of
                  Right newSlotConfiguration ->
                    setSlotConfigurationFFI relayKey newSlotConfiguration *> pure result

                  _ ->
                    pure result
              )
          <#> map (\newSlotConfiguration -> streamIn{ slotConfiguration = Just $ spy "stream relay got slot config" newSlotConfiguration })

    handleFailure (Left failure) =
      logWarning "Failed" failure <#> const source

    handleFailure (Right newSource) =
      pure newSource

--------------------------------------------------------------------------------
-- Stream Relay Utilities
--------------------------------------------------------------------------------
fetchIngestAggregatorSlotConfiguration :: Server -> SlotId -> SlotRole -> Effect SlotConfiguration
fetchIngestAggregatorSlotConfiguration aggregator (SlotId slotId) streamRole =
  fetchSlotConfiguration configURL

  where
    streamRoleString =
      case streamRole of
        Primary ->
          "primary"
        Backup ->
          "backup"

    configURL = makeUrlWithPath aggregator $ "/api/agents/ingestAggregator/" <> (show slotId) <> "/" <> streamRoleString <> "/slot"

fetchStreamRelaySlotConfiguration :: Server -> SlotId -> SlotRole -> Effect SlotConfiguration
fetchStreamRelaySlotConfiguration relay (SlotId slotId) streamRole =
  fetchSlotConfiguration configURL

  where
    streamRoleString =
      case streamRole of
        Primary ->
          "primary"
        Backup ->
          "backup"

    configURL = makeUrlWithPath relay $ "/api/agents/relay/" <> (show slotId) <> "/" <> streamRoleString <> "/slot"

fetchSlotConfiguration :: Url -> Effect SlotConfiguration
fetchSlotConfiguration url =
  do
    response <- SpudGun.getJson url

    pure $ unsafePartial $ fromRight $ (SpudGun.bodyToJSON response)

--------------------------------------------------------------------------------
-- General Utilities
--------------------------------------------------------------------------------
extractServedByHeader :: SpudResponse -> Maybe ServerAddress
extractServedByHeader (SpudResponse _statusCode headers _body) =
  headers
    # find (\tuple -> fst tuple == "x-servedby")
    # map  (\tuple -> ServerAddress (snd tuple))

chainEither :: forall l a b. (a -> Effect (Either l b)) -> Either l a -> Effect (Either l b)
chainEither = either (pure <<< Left)

mapLeft :: forall a b r. (a -> b) -> Either a r -> Either b r
mapLeft leftMapper = either (Left <<< leftMapper) (Right)

--------------------------------------------------------------------------------
-- Log helpers
--------------------------------------------------------------------------------
domains :: List Atom
domains = atom <$> (show Agent.StreamRelay :  "Instance" : nil)

logInfo :: forall a. Logger a
logInfo = domainLog Logger.info

logWarning :: forall a. Logger a
logWarning = domainLog Logger.warning

logError :: forall a. Logger a
logError = domainLog Logger.error

domainLog :: forall a. Logger {domain :: List Atom, misc :: a} -> Logger a
domainLog = Logger.doLog domains
