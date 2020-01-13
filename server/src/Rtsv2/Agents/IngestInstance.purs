module Rtsv2.Agents.IngestInstance
  ( startLink
  , isActive
  , stopIngest
  ) where

import Prelude

import Bus as Bus
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe, fromMaybe')
import Data.Newtype (unwrap, wrap)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Erl.Atom (atom)
import Erl.Data.List (nil, (:))
import Erl.Utils (Milliseconds)
import Foreign (Foreign)
import Logger (spy)
import Logger as Logger
import Pinto (ServerName, StartLinkResult)
import Pinto.Gen (CallResult(..), CastResult(..))
import Pinto.Gen as Gen
import Pinto.Timer as Timer
import Record as Record
import Rtsv2.Agents.IngestAggregatorInstance as IngestAggregatorInstance
import Rtsv2.Agents.IngestAggregatorInstanceSup as IngestAggregatorInstanceSup
import Rtsv2.Agents.IntraPoP (IntraPoPBusMessage(..))
import Rtsv2.Agents.IntraPoP as IntraPoP
import Rtsv2.Audit as Audit
import Rtsv2.Config (PoPDefinitionConfig)
import Rtsv2.Config as Config
import Rtsv2.Load as Load
import Rtsv2.Names as Names
import Rtsv2.PoPDefinition as PoPDefinition
import Shared.Agent as Agent
import Shared.LlnwApiTypes (StreamDetails)
import Shared.Stream (StreamId(..), StreamVariantId(..), toStreamId)
import Shared.Types (ServerAddress(..))
import Simple.JSON as JSON
import SpudGun as SpudGun

serverName :: StreamVariantId -> ServerName State Msg
serverName streamVariantId = Names.ingestInstanceName streamVariantId

data Msg
   = InformAggregator
   | IntraPoPBus IntraPoP.IntraPoPBusMessage

type State
  = { thisNode :: ServerAddress
    , aggregatorRetryTime :: Milliseconds
    , streamId :: StreamId
    , streamVariantId :: StreamVariantId
    , streamDetails :: StreamDetails
    }

startLink :: Tuple StreamDetails StreamVariantId -> Effect StartLinkResult
startLink (Tuple streamDetails streamVariantId) = Gen.startLink (serverName streamVariantId) (init streamDetails streamVariantId) handleInfo

isActive :: StreamVariantId -> Effect Boolean
isActive streamVariantId = Names.isRegistered (serverName streamVariantId)

stopIngest :: StreamVariantId -> Effect Unit
stopIngest streamVariantId =
  Gen.doCall (serverName streamVariantId) \state -> do
    -- TODO - single ingest can't stop the aggregator - just remove this variant and allow the aggregator to stop when it deems fit
    _ <- IngestAggregatorInstance.stopAggregator (toStreamId streamVariantId)
    _ <- Audit.ingestStop streamVariantId
    pure $ CallStop unit state

init :: StreamDetails -> StreamVariantId -> Effect State
init streamDetails streamVariantId = do
  _ <- logInfo "Ingest starting" {streamVariantId: streamVariantId}
  thisNode <- PoPDefinition.thisNode
  {intraPoPLatencyMs} <- Config.globalConfig
  _ <- Bus.subscribe (serverName streamVariantId) IntraPoP.bus IntraPoPBus
  _ <- Audit.ingestStart streamVariantId
  _ <- Timer.sendAfter (serverName streamVariantId) 0 InformAggregator
  pure { thisNode
       , streamDetails
       , streamVariantId
       , streamId: toStreamId streamVariantId
       , aggregatorRetryTime: wrap intraPoPLatencyMs}

handleInfo :: Msg -> State -> Effect (CastResult State)
handleInfo msg state = case msg of
  InformAggregator -> do
    state2 <- informAggregator state
    pure $ CastNoReply state2
  IntraPoPBus (IngestAggregatorExited streamId serverAddress) -> do
    _ <- logInfo "exit" {streamId, serverAddress, thisStreamId: state.streamId}
    state2 <- handleAggregatorExit streamId state
    pure $ CastNoReply state2

informAggregator :: State -> Effect State
informAggregator state@{streamDetails, streamVariantId, thisNode, aggregatorRetryTime} = do
  maybeAggregator <- getAggregator streamDetails streamVariantId
  maybeVariantAdded <- sequence ((addVariant thisNode streamVariantId) <$> maybeAggregator)
  case fromMaybe false maybeVariantAdded of
    true -> pure state
    false -> do
      _ <- Timer.sendAfter (serverName streamVariantId) (unwrap aggregatorRetryTime) InformAggregator
      pure state

handleAggregatorExit :: StreamId -> State -> Effect State
handleAggregatorExit exitedStreamId state@{streamId, streamVariantId, aggregatorRetryTime}
  | exitedStreamId == streamId = do
      _ <- Timer.sendAfter (serverName streamVariantId) 0 InformAggregator
      pure state
  | otherwise =
      pure state

addVariant :: ServerAddress -> StreamVariantId -> ServerAddress -> Effect Boolean
addVariant thisNode streamVariantId aggregatorAddress
  | aggregatorAddress == thisNode = do
    _ <- IngestAggregatorInstance.addVariant streamVariantId
    pure true
  | otherwise = do
    let
      -- TODO - functions to make URLs from ServerAddress
      ServerAddress addr = aggregatorAddress
      StreamVariantId streamId variantId = streamVariantId
      url = "http://" <> addr <> ":3000/api/agents/ingestAggregator/" <> streamId <> "/activeIngests/" <> variantId
    restResult <- SpudGun.post url (JSON.writeJSON thisNode)
    case restResult of
      Left _ -> pure $ false
      Right _ -> pure $ true

getAggregator :: StreamDetails -> StreamVariantId -> Effect (Maybe ServerAddress)
getAggregator streamDetails streamVariantId = do
  maybeAggregator <- IntraPoP.whereIsIngestAggregator streamVariantId
  case maybeAggregator of
    Just aggregator ->
      pure maybeAggregator
    Nothing ->
      launchLocalOrRemote streamDetails streamVariantId

launchLocalOrRemote :: StreamDetails -> StreamVariantId -> Effect (Maybe ServerAddress)
launchLocalOrRemote streamDetails streamVariantId = do
  currentLoad <- Load.load
  if
    currentLoad < (wrap 50.0) then do
      _ <- IngestAggregatorInstanceSup.startAggregator streamDetails
      Just <$> PoPDefinition.thisNode
    else
      launchRemote streamDetails streamVariantId

launchRemote :: StreamDetails -> StreamVariantId -> Effect (Maybe ServerAddress)
launchRemote streamDetails streamVariantId = do
  candidate <- IntraPoP.getIdleServer
  case candidate of
    Nothing ->
      pure Nothing
    Just (ServerAddress addr) -> do
      let
        -- TODO - functions to make URLs from ServerAddress
        url = "http://" <> addr <> ":3000/api/agents/ingestAggregator"
      restResult <- SpudGun.post url (JSON.writeJSON streamDetails)
      case restResult of
        Left _ -> pure Nothing
        Right _ -> pure candidate

logInfo :: forall a. String -> a -> Effect Foreign
logInfo msg metaData = Logger.info msg (Record.merge { domain: ((atom (show Agent.Ingest)) : nil) } { misc: metaData })
