module Rtsv2.Agents.IngestAggregatorInstance
  ( startLink
  , isAvailable
  , addVariant
  , addRemoteVariant
  , removeVariant
  , getState
  ) where

import Prelude

import Data.Newtype (unwrap, wrap)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Erl.Atom (Atom, atom)
import Erl.Data.List (List, nil, (:))
import Erl.Data.Map (Map, delete, insert, size, toUnfoldable)
import Erl.Data.Map as Map
import Erl.Data.Tuple (Tuple2, tuple2)
import Foreign (Foreign)
import Logger (Logger)
import Logger as Logger
import Pinto (ServerName, StartLinkResult, isRegistered)
import Pinto.Gen (CallResult(..), CastResult(..))
import Pinto.Gen as Gen
import Pinto.Timer as Timer
import Rtsv2.Agents.IntraPoP (announceLocalAggregatorIsAvailable, announceLocalAggregatorStopped)
import Rtsv2.Config as Config
import Rtsv2.Names as Names
import Rtsv2.PoPDefinition as PoPDefinition
import Rtsv2.Router.Endpoint (Endpoint(..))
import Rtsv2.Router.Endpoint as RoutingEndpoint
import Rtsv2.Router.Parser as Routing
import Shared.Agent as Agent
import Shared.LlnwApiTypes (StreamDetails)
import Shared.Stream (AggregatorKey(..), IngestKey(..), StreamVariant, ingestKeyToAggregatorKey, ingestKeyToVariant)
import Shared.Types (ServerAddress, extractAddress)
import Shared.Types.Agent.State as PublicState

foreign import startWorkflowImpl :: String -> Array (Tuple2 IngestKey String) -> Effect Foreign
foreign import addLocalVariantImpl :: Foreign -> IngestKey -> Effect Unit
foreign import addRemoteVariantImpl :: Foreign -> IngestKey -> String -> Effect Unit
foreign import removeVariantImpl :: Foreign -> IngestKey -> Effect Unit

type State
  = { config :: Config.IngestAggregatorAgentConfig
    , thisAddress :: ServerAddress
    , aggregatorKey :: AggregatorKey
    , streamDetails :: StreamDetails
    , activeStreamVariants :: Map StreamVariant ServerAddress
    , workflow :: Foreign
    }

data Msg
  = MaybeStop

isAvailable :: AggregatorKey -> Effect Boolean
isAvailable aggregatorKey = do
  bool <- isRegistered (serverName aggregatorKey)
  pure bool

serverName :: AggregatorKey -> ServerName State Msg
serverName = Names.ingestAggregatorInstanceName


serverNameFromIngestKey :: IngestKey -> ServerName State Msg
serverNameFromIngestKey = serverName <<< ingestKeyToAggregatorKey

addVariant :: IngestKey -> Effect Unit
addVariant ingestKey = Gen.doCall (serverNameFromIngestKey ingestKey)
  \state@{thisAddress, activeStreamVariants, workflow} -> do
    logInfo "Ingest variant added" {ingestKey}
    addLocalVariantImpl workflow ingestKey
    pure $ CallReply unit state{activeStreamVariants = insert (ingestKeyToVariant ingestKey) thisAddress activeStreamVariants}

addRemoteVariant :: IngestKey -> ServerAddress -> Effect Unit
addRemoteVariant ingestKey@(IngestKey streamId streamRole streamVariant)  remoteServer = Gen.doCall (serverNameFromIngestKey ingestKey)
  \state@{activeStreamVariants, workflow} -> do
    logInfo "Remote ingest variant added" {ingestKey, source: remoteServer}
    let
      path = Routing.printUrl RoutingEndpoint.endpoint (IngestInstanceLlwpE streamId streamRole streamVariant)
      url = "http://" <> (unwrap remoteServer) <> ":3000" <> path
    addRemoteVariantImpl workflow ingestKey url
    pure $ CallReply unit state{activeStreamVariants = insert (ingestKeyToVariant ingestKey) remoteServer activeStreamVariants}

removeVariant :: IngestKey -> Effect Unit
removeVariant ingestKey@(IngestKey _ _ streamVariant )  = Gen.doCall (serverName (ingestKeyToAggregatorKey ingestKey))
  \state@{activeStreamVariants, aggregatorKey, workflow, config:{shutdownLingerTimeMs}} -> do
  let
    newActiveStreamVariants = delete streamVariant activeStreamVariants
  removeVariantImpl workflow ingestKey
  if (size newActiveStreamVariants) == 0 then do
    void $ Timer.sendAfter (serverName aggregatorKey) shutdownLingerTimeMs MaybeStop
    pure unit
  else
    pure unit
  pure $ CallReply unit state{activeStreamVariants = newActiveStreamVariants}

getState :: AggregatorKey -> Effect (PublicState.IngestAggregator List)
getState aggregatorKey@(AggregatorKey _streamId streamRole) = Gen.call (serverName aggregatorKey)
  \state@{streamDetails, activeStreamVariants} ->
  CallReply {role: streamRole, streamDetails, activeStreamVariants: (\(Tuple streamVariant serverAddress) -> {streamVariant, serverAddress}) <$> (toUnfoldable activeStreamVariants)} state

startLink :: StreamDetails -> Effect StartLinkResult
startLink streamDetails@{slot : {name}} = Gen.startLink (serverName $ streamDetailsToAggregatorKey streamDetails) (init streamDetails) handleInfo

init :: StreamDetails -> Effect State
init streamDetails = do
  logInfo "Ingest Aggregator starting" {aggregatorKey, streamDetails}
  config <- Config.ingestAggregatorAgentConfig
  thisServer <- PoPDefinition.getThisServer
  announceLocalAggregatorIsAvailable aggregatorKey
  workflow <- startWorkflowImpl streamDetails.slot.name $ mkKey <$> streamDetails.slot.profiles
  pure { config : config
       , thisAddress : extractAddress thisServer
       , aggregatorKey
       , streamDetails
       , activeStreamVariants : Map.empty
       , workflow
       }
  where
    mkKey p = tuple2 (IngestKey (wrap streamDetails.slot.name) streamDetails.role (wrap p.streamName)) p.streamName
    aggregatorKey = (AggregatorKey  (wrap streamDetails.slot.name) streamDetails.role)

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
