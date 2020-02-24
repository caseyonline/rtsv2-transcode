module Rtsv2.Agents.IngestAggregatorInstance
  ( startLink
  , isAvailable
  , addIngest
  , addRemoteIngest
  , removeIngest
  , registerRelay
  , getState
  , slotConfiguration
  ) where

import Prelude

import Data.Maybe (Maybe)
import Data.Newtype (unwrap)
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
import Rtsv2.Agents.SlotTypes (SlotConfiguration)
import Rtsv2.Agents.StreamRelayTypes (RegisterRelayPayload)
import Rtsv2.Config as Config
import Rtsv2.Names as Names
import Rtsv2.PoPDefinition as PoPDefinition
import Rtsv2.Router.Endpoint (Endpoint(..))
import Rtsv2.Router.Endpoint as RoutingEndpoint
import Rtsv2.Router.Parser as Routing
import Shared.Agent as Agent
import Shared.LlnwApiTypes (SlotProfile(..), StreamDetails)
import Shared.Stream (AggregatorKey(..), IngestKey(..), SlotId(..), SlotRole, ProfileName, ingestKeyToAggregatorKey, ingestKeyToProfileName)
import Shared.Types (ServerAddress, extractAddress)
import Shared.Types.Agent.State as PublicState

-- TODO: proper type for handle, as done in other places
type WorkflowHandle = Foreign
foreign import startWorkflowImpl :: Int -> Array (Tuple3 IngestKey String String) -> Effect WorkflowHandle
foreign import addLocalIngestImpl :: WorkflowHandle -> IngestKey -> Effect Unit
foreign import addRemoteIngestImpl :: WorkflowHandle -> IngestKey -> String -> Effect Unit
foreign import removeIngestImpl :: WorkflowHandle -> IngestKey -> Effect Unit
foreign import registerStreamRelayImpl :: WorkflowHandle -> String -> Int -> Effect Unit
foreign import slotConfigurationImpl :: Int -> Effect (Maybe SlotConfiguration)

type State
  = { config :: Config.IngestAggregatorAgentConfig
    , thisAddress :: ServerAddress
    , aggregatorKey :: AggregatorKey
    , streamDetails :: StreamDetails
    , activeProfileNames :: Map ProfileName ServerAddress
    , workflowHandle :: WorkflowHandle
    }

data Msg
  = MaybeStop

isAvailable :: AggregatorKey -> Effect Boolean
isAvailable aggregatorKey = do
  bool <- isRegistered (serverName aggregatorKey)
  pure bool

payloadToAggregatorKey :: forall r. { slotId :: SlotId, streamRole :: SlotRole | r } -> AggregatorKey
payloadToAggregatorKey payload = AggregatorKey payload.slotId payload.streamRole

serverName :: AggregatorKey -> ServerName State Msg
serverName = Names.ingestAggregatorInstanceName

serverNameFromIngestKey :: IngestKey -> ServerName State Msg
serverNameFromIngestKey = serverName <<< ingestKeyToAggregatorKey

addIngest :: IngestKey -> Effect Unit
addIngest ingestKey = Gen.doCall (serverNameFromIngestKey ingestKey)
  \state@{thisAddress, activeProfileNames, workflowHandle} -> do
    logInfo "Ingest added" {ingestKey}
    addLocalIngestImpl workflowHandle ingestKey
    pure $ CallReply unit state{activeProfileNames = insert (ingestKeyToProfileName ingestKey) thisAddress activeProfileNames}

addRemoteIngest :: IngestKey -> ServerAddress -> Effect Unit
addRemoteIngest ingestKey@(IngestKey streamId streamRole profileName)  remoteServer = Gen.doCall (serverNameFromIngestKey ingestKey)
  \state@{activeProfileNames, workflowHandle} -> do
    logInfo "Remote ingest added" {ingestKey, source: remoteServer}
    let
      path = Routing.printUrl RoutingEndpoint.endpoint (IngestInstanceLlwpE streamId streamRole profileName)
      url = "http://" <> (unwrap remoteServer) <> ":3000" <> path
    addRemoteIngestImpl workflowHandle ingestKey url
    pure $ CallReply unit state{activeProfileNames = insert (ingestKeyToProfileName ingestKey) remoteServer activeProfileNames}

removeIngest :: IngestKey -> Effect Unit
removeIngest ingestKey@(IngestKey _ _ profileName )  = Gen.doCall (serverName (ingestKeyToAggregatorKey ingestKey))
  \state@{activeProfileNames, aggregatorKey, workflowHandle, config:{shutdownLingerTimeMs}} -> do
  let
    newActiveProfileNames = delete profileName activeProfileNames
  removeIngestImpl workflowHandle ingestKey
  if (size newActiveProfileNames) == 0 then do
    void $ Timer.sendAfter (serverName aggregatorKey) shutdownLingerTimeMs MaybeStop
    pure unit
  else
    pure unit
  pure $ CallReply unit state{activeProfileNames = newActiveProfileNames}

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
  \state@{streamDetails, activeProfileNames} ->
  CallReply {role: streamRole, streamDetails, activeProfiles: (\(Tuple profileName serverAddress) -> {profileName, serverAddress}) <$> (toUnfoldable activeProfileNames)} state

slotConfiguration :: AggregatorKey -> Effect (Maybe SlotConfiguration)
slotConfiguration (AggregatorKey (SlotId slotId) _streamRole) =
  -- TODO: the key is what the slot config should be keyed on...
  slotConfigurationImpl slotId

startLink :: StreamDetails -> Effect StartLinkResult
startLink streamDetails = Gen.startLink (serverName $ streamDetailsToAggregatorKey streamDetails) (init streamDetails) handleInfo

init :: StreamDetails -> Effect State
init streamDetails = do
  logInfo "Ingest Aggregator starting" {aggregatorKey, streamDetails}
  config <- Config.ingestAggregatorAgentConfig
  thisServer <- PoPDefinition.getThisServer
  announceLocalAggregatorIsAvailable aggregatorKey
  workflowHandle <- startWorkflowImpl (unwrap streamDetails.slot.id) $ mkKey <$> streamDetails.slot.profiles
  pure { config : config
       , thisAddress : extractAddress thisServer
       , aggregatorKey
       , streamDetails
       , activeProfileNames : Map.empty
       , workflowHandle
       }
  where
    mkKey (SlotProfile p) = tuple3 (IngestKey streamDetails.slot.id streamDetails.role p.name) p.rtmpStreamName (unwrap p.name)
    aggregatorKey = streamDetailsToAggregatorKey streamDetails

streamDetailsToAggregatorKey :: StreamDetails -> AggregatorKey
streamDetailsToAggregatorKey streamDetails =
  AggregatorKey streamDetails.slot.id streamDetails.role

handleInfo :: Msg -> State -> Effect (CastResult State)
handleInfo msg state@{activeProfileNames, aggregatorKey} =
  case msg of
    MaybeStop
      | size activeProfileNames == 0 -> do
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
