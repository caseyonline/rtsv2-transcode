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
import Pinto (ServerName, StartLinkResult)
import Pinto.Gen (CallResult(..), CastResult(..))
import Pinto.Gen as Gen
import Pinto.Timer as Timer
import Rtsv2.Agents.IntraPoP (announceStreamIsAvailable, announceStreamStopped)
import Rtsv2.Config as Config
import Rtsv2.Names as Names
import Rtsv2.PoPDefinition as PoPDefinition
import Rtsv2.Router.Endpoint (Endpoint(..))
import Rtsv2.Router.Endpoint as RoutingEndpoint
import Rtsv2.Router.Parser as Routing
import Shared.Agent as Agent
import Shared.LlnwApiTypes (StreamDetails)
import Shared.Stream (StreamAndVariant(..), StreamId(..), StreamVariant, toStreamId, toVariant)
import Shared.Types (IngestAggregatorPublicState, ServerAddress, extractAddress)

foreign import startWorkflowImpl :: String -> Array (Tuple2 StreamAndVariant String) -> Effect Foreign
foreign import addLocalVariantImpl :: Foreign -> StreamAndVariant -> Effect Unit
foreign import addRemoteVariantImpl :: Foreign -> StreamAndVariant -> String -> Effect Unit
foreign import removeVariantImpl :: Foreign -> StreamAndVariant -> Effect Unit

type State
  = { config :: Config.IngestAggregatorAgentConfig
    , thisAddress :: ServerAddress
    , streamId :: StreamId
    , streamDetails :: StreamDetails
    , activeStreamVariants :: Map StreamVariant ServerAddress
    , workflow :: Foreign
    }

data Msg
  = Tick
  | MaybeStop

isAvailable :: StreamId -> Effect Boolean
isAvailable streamId = do
  bool <- Names.isRegistered (serverName streamId)
  pure bool

serverName :: StreamId -> ServerName State Msg
serverName = Names.ingestAggregatorInstanceName

addVariant :: StreamAndVariant -> Effect Unit
addVariant streamAndVariant = Gen.doCall (serverName (toStreamId streamAndVariant))
  \state@{thisAddress, activeStreamVariants, workflow} -> do
    _ <- logInfo "Ingest variant added" {streamId: streamAndVariant}
    _ <- addLocalVariantImpl workflow streamAndVariant
    pure $ CallReply unit state{activeStreamVariants = insert (toVariant streamAndVariant) thisAddress activeStreamVariants}

addRemoteVariant :: StreamAndVariant -> ServerAddress -> Effect Unit
addRemoteVariant streamAndVariant remoteServer = Gen.doCall (serverName (toStreamId streamAndVariant))
  \state@{activeStreamVariants, workflow} -> do
    _ <- logInfo "Remote ingest variant added" {streamId: streamAndVariant, source: remoteServer}
    let
      path = Routing.printUrl RoutingEndpoint.endpoint (IngestInstanceLlwpE (toStreamId streamAndVariant) (toVariant streamAndVariant))
      url = "http://" <> (unwrap remoteServer) <> ":3000" <> path
    _ <- addRemoteVariantImpl workflow streamAndVariant url
    pure $ CallReply unit state{activeStreamVariants = insert (toVariant streamAndVariant) remoteServer activeStreamVariants}

removeVariant :: StreamAndVariant -> Effect Unit
removeVariant streamAndVariant = Gen.doCall (serverName (toStreamId streamAndVariant))
  \state@{activeStreamVariants, streamId, workflow, config:{shutdownLingerTimeMs}} -> do
  let
    newActiveStreamVariants = delete (toVariant streamAndVariant) activeStreamVariants
  _ <- removeVariantImpl workflow streamAndVariant
  _ <- if (size newActiveStreamVariants) == 0 then do
         _ <- Timer.sendAfter (serverName streamId) shutdownLingerTimeMs MaybeStop
         pure unit
       else
         pure unit
  pure $ CallReply unit state{activeStreamVariants = newActiveStreamVariants}

getState :: StreamId -> Effect (IngestAggregatorPublicState)
getState streamId = Gen.call (serverName streamId)
  \state@{streamDetails, activeStreamVariants} ->
  CallReply {streamDetails, activeStreamVariants: (\(Tuple streamVariant serverAddress) -> {streamVariant, serverAddress}) <$>(toUnfoldable activeStreamVariants)} state

startLink :: StreamDetails -> Effect StartLinkResult
startLink streamDetails@{slot : {name}} = Gen.startLink (serverName (StreamId name)) (init streamDetails) handleInfo

init :: StreamDetails -> Effect State
init streamDetails = do
  _ <- logInfo "Ingest Aggregator starting" {streamId, streamDetails}
  config <- Config.ingestAggregatorAgentConfig
  thisServer <- PoPDefinition.getThisServer
  _ <- Timer.sendEvery (serverName streamId) config.streamAvailableAnnounceMs Tick
  _ <- announceStreamIsAvailable streamId
  workflow <- startWorkflowImpl streamDetails.slot.name ((\p -> tuple2 (StreamAndVariant (wrap streamDetails.slot.name) (wrap p.streamName)) p.streamName) <$> streamDetails.slot.profiles)
  pure { config : config
       , thisAddress : extractAddress thisServer
       , streamId
       , streamDetails
       , activeStreamVariants : Map.empty
       , workflow
       }
  where
    streamId = StreamId streamDetails.slot.name

handleInfo :: Msg -> State -> Effect (CastResult State)
handleInfo msg state@{activeStreamVariants, streamId} =
  case msg of
    Tick -> CastNoReply <$> handleTick state
    MaybeStop
      | size activeStreamVariants == 0 -> do
        _ <- logInfo "Ingest Aggregator stopping" {streamId: streamId}
        _ <- announceStreamStopped streamId
        pure $ CastStop state
      | otherwise -> pure $ CastNoReply state

handleTick :: State -> Effect State
handleTick state@{streamId} = do
  _ <- announceStreamIsAvailable streamId
  pure state

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
