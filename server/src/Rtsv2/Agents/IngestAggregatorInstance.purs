module Rtsv2.Agents.IngestAggregatorInstance
  ( startLink
  , isAvailable
  , addVariant
  , addRemoteVariant
  , removeVariant
  , registerRelay
  , getState
  , slotConfiguration
  ) where

import Prelude

import Data.Newtype (unwrap, wrap)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Erl.Atom (Atom, atom)
import Erl.Data.List (List, nil, (:))
import Erl.Data.Map (Map, delete, insert, size, toUnfoldable)
import Erl.Data.Map as Map
import Erl.Data.Tuple (Tuple3, tuple3)
import Foreign (Foreign)
import Logger (Logger, spy)
import Logger as Logger
import Pinto (ServerName, StartLinkResult, isRegistered)
import Pinto.Gen (CallResult(..), CastResult(..))
import Pinto.Gen as Gen
import Pinto.Timer as Timer
import Rtsv2.Agents.IntraPoP (announceLocalAggregatorIsAvailable, announceLocalAggregatorStopped)
import Rtsv2.Agents.StreamRelayTypes (RegisterRelayPayload)
import Rtsv2.Agents.SlotTypes (SlotConfiguration)
import Rtsv2.Config as Config
import Rtsv2.Names as Names
import Rtsv2.PoPDefinition as PoPDefinition
import Rtsv2.Router.Endpoint (Endpoint(..))
import Rtsv2.Router.Endpoint as RoutingEndpoint
import Rtsv2.Router.Parser as Routing
import Shared.Agent as Agent
import Shared.LlnwApiTypes (StreamDetails)
import Shared.Stream (AggregatorKey(..), IngestKey(..), StreamId(..), StreamRole, StreamVariant, ingestKeyToAggregatorKey, ingestKeyToVariant)
import Shared.Types (ServerAddress, extractAddress)
import Shared.Types.Agent.State as PublicState

-- TODO: proper type for handle, as done in other places
type WorkflowHandle = Foreign
foreign import startWorkflowImpl :: String -> Array (Tuple3 IngestKey String String) -> Effect WorkflowHandle
foreign import addLocalVariantImpl :: WorkflowHandle -> IngestKey -> Effect Unit
foreign import addRemoteVariantImpl :: WorkflowHandle -> IngestKey -> String -> Effect Unit
foreign import removeVariantImpl :: WorkflowHandle -> IngestKey -> Effect Unit
foreign import registerStreamRelayImpl :: WorkflowHandle -> String -> Int -> Effect Unit
foreign import slotConfigurationImpl :: String -> Effect (Maybe SlotConfiguration)

type State
  = { config :: Config.IngestAggregatorAgentConfig
    , thisAddress :: ServerAddress
    , aggregatorKey :: AggregatorKey
    , streamDetails :: StreamDetails
    , activeStreamVariants :: Map StreamVariant ServerAddress
    , workflowHandle :: WorkflowHandle
    }

data Msg
  = MaybeStop

isAvailable :: AggregatorKey -> Effect Boolean
isAvailable aggregatorKey = do
  bool <- isRegistered (serverName aggregatorKey)
  pure bool

payloadToAggregatorKey :: forall r.
  { streamId :: StreamId
  , streamRole :: StreamRole
  | r
  }
  -> AggregatorKey
payloadToAggregatorKey payload = AggregatorKey payload.streamId payload.streamRole

serverName :: AggregatorKey -> ServerName State Msg
serverName = Names.ingestAggregatorInstanceName

serverNameFromIngestKey :: IngestKey -> ServerName State Msg
serverNameFromIngestKey = serverName <<< ingestKeyToAggregatorKey

addVariant :: IngestKey -> Effect Unit
addVariant ingestKey = Gen.doCall (serverNameFromIngestKey ingestKey)
  \state@{thisAddress, activeStreamVariants, workflowHandle} -> do
    logInfo "Ingest variant added" {ingestKey}
    addLocalVariantImpl workflowHandle ingestKey
    pure $ CallReply unit state{activeStreamVariants = insert (ingestKeyToVariant ingestKey) thisAddress activeStreamVariants}

addRemoteVariant :: IngestKey -> ServerAddress -> Effect Unit
addRemoteVariant ingestKey@(IngestKey streamId streamRole streamVariant)  remoteServer = Gen.doCall (serverNameFromIngestKey ingestKey)
  \state@{activeStreamVariants, workflowHandle} -> do
    logInfo "Remote ingest variant added" {ingestKey, source: remoteServer}
    let
      path = Routing.printUrl RoutingEndpoint.endpoint (IngestInstanceLlwpE streamId streamRole streamVariant)
      url = "http://" <> (unwrap remoteServer) <> ":3000" <> path
    addRemoteVariantImpl workflowHandle ingestKey url
    pure $ CallReply unit state{activeStreamVariants = insert (ingestKeyToVariant ingestKey) remoteServer activeStreamVariants}

removeVariant :: IngestKey -> Effect Unit
removeVariant ingestKey@(IngestKey _ _ streamVariant )  = Gen.doCall (serverName (ingestKeyToAggregatorKey ingestKey))
  \state@{activeStreamVariants, aggregatorKey, workflowHandle, config:{shutdownLingerTimeMs}} -> do
  let
    newActiveStreamVariants = delete streamVariant activeStreamVariants
  removeVariantImpl workflowHandle ingestKey
  if (size newActiveStreamVariants) == 0 then do
    void $ Timer.sendAfter (serverName aggregatorKey) shutdownLingerTimeMs MaybeStop
    pure unit
  else
    pure unit
  pure $ CallReply unit state{activeStreamVariants = newActiveStreamVariants}

registerRelay :: RegisterRelayPayload -> Effect Unit
registerRelay payload@{deliverTo} = Gen.doCast (serverName $ payloadToAggregatorKey payload) doRegisterRelay
  where
    doRegisterRelay state =
      do
        let _ = spy "Registering with ingest aggregator instance" payload
        registerStreamRelayImpl state.workflowHandle (deliverTo.server # unwrap # _.address # unwrap) (deliverTo.port)
        pure $ Gen.CastNoReply state

getState :: AggregatorKey -> Effect (PublicState.IngestAggregator List)
getState aggregatorKey@(AggregatorKey _streamId streamRole) = Gen.call (serverName aggregatorKey)
  \state@{streamDetails, activeStreamVariants} ->
  CallReply {role: streamRole, streamDetails, activeStreamVariants: (\(Tuple streamVariant serverAddress) -> {streamVariant, serverAddress}) <$> (toUnfoldable activeStreamVariants)} state

slotConfiguration :: AggregatorKey -> Effect (Maybe SlotConfiguration)
slotConfiguration (AggregatorKey (StreamId streamId) _streamRole) =
  -- TODO: the key is what the slot config should be keyed on...
  slotConfigurationImpl streamId

startLink :: StreamDetails -> Effect StartLinkResult
startLink streamDetails@{slot : {name}} = Gen.startLink (serverName $ streamDetailsToAggregatorKey streamDetails) (init streamDetails) handleInfo

init :: StreamDetails -> Effect State
init streamDetails = do
  logInfo "Ingest Aggregator starting" {aggregatorKey, streamDetails}
  config <- Config.ingestAggregatorAgentConfig
  thisServer <- PoPDefinition.getThisServer
  announceLocalAggregatorIsAvailable aggregatorKey
  workflowHandle <- startWorkflowImpl streamDetails.slot.name $ mkKey <$> streamDetails.slot.profiles
  pure { config : config
       , thisAddress : extractAddress thisServer
       , aggregatorKey
       , streamDetails
       , activeStreamVariants : Map.empty
       , workflowHandle
       }
  where
    mkKey p = tuple3 (IngestKey (wrap streamDetails.slot.name) streamDetails.role (wrap p.name)) p.streamName p.name
    aggregatorKey = streamDetailsToAggregatorKey streamDetails

streamDetailsToAggregatorKey :: StreamDetails -> AggregatorKey
streamDetailsToAggregatorKey streamDetails =
  AggregatorKey (wrap streamDetails.slot.name) streamDetails.role

handleInfo :: Msg -> State -> Effect (CastResult State)
handleInfo msg state@{activeStreamVariants, aggregatorKey} =
  case msg of
    MaybeStop
      | size activeStreamVariants == 0 -> do
        logInfo "Ingest Aggregator stopping" {aggregatorKey}
        announceLocalAggregatorStopped aggregatorKey
        pure $ CastStop state
      | otherwise -> pure $ CastNoReply state

--------------------------------------------------------------------------------
-- Log helpers
--------------------------------------------------------------------------------
domains :: List Atom
domains = atom <$> (show Agent.IngestAggregator :  "Instance" : nil)

logInfo :: forall a. Logger a
logInfo = domainLog Logger.info

logWarning :: forall a. Logger a
logWarning = domainLog Logger.warning

domainLog :: forall a. Logger {domain :: List Atom, misc :: a} -> Logger a
domainLog = Logger.doLog domains
