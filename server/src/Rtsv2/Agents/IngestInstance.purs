module Rtsv2.Agents.IngestInstance
  ( startLink
  , isActive
  , stopIngest
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Erl.Atom (atom)
import Erl.Data.List (nil, (:))
import Foreign (Foreign)
import Logger (spy)
import Logger as Logger
import Pinto (ServerName, StartLinkResult)
import Pinto.Gen (CallResult(..))
import Pinto.Gen as Gen
import Record as Record
import Rtsv2.Agents.IngestAggregatorInstance as IngestAggregatorInstance
import Rtsv2.Agents.IngestAggregatorInstanceSup as IngestAggregatorInstanceSup
import Rtsv2.Agents.IntraPoP as IntraPoP
import Rtsv2.Audit as Audit
import Rtsv2.Config (PoPDefinitionConfig)
import Rtsv2.Load as Load
import Rtsv2.Names as Names
import Rtsv2.PoPDefinition as PoPDefinition
import Shared.Agent as Agent
import Shared.LlnwApiTypes (StreamDetails)
import Shared.Stream (StreamVariantId(..), toStreamId)
import Shared.Types (ServerAddress(..))
import Simple.JSON as JSON
import SpudGun as SpudGun

type State
  = { }

isActive :: StreamVariantId -> Effect Boolean
isActive streamVariantId = Names.isRegistered (serverName streamVariantId)

serverName :: StreamVariantId -> ServerName State Unit
serverName streamVariantId = Names.ingestInstanceName streamVariantId

startLink :: Tuple StreamDetails StreamVariantId -> Effect StartLinkResult
startLink (Tuple streamDetails streamVariantId) = Gen.startLink (serverName streamVariantId) (init streamDetails streamVariantId) Gen.defaultHandleInfo

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
  _ <- Audit.ingestStart streamVariantId
  maybeAggregator <- getAggregator streamDetails streamVariantId
  _ <- addVariant thisNode streamVariantId maybeAggregator
  pure {}

addVariant :: ServerAddress -> StreamVariantId -> Maybe ServerAddress -> Effect Unit
addVariant thisNode streamVariantId Nothing = pure unit
addVariant thisNode streamVariantId (Just aggregatorAddress)
  | aggregatorAddress == thisNode = do
    _ <- IngestAggregatorInstance.addVariant streamVariantId
    pure unit
  | otherwise = do
    let
      -- TODO - functions to make URLs from ServerAddress
      ServerAddress addr = aggregatorAddress
      StreamVariantId streamId variantId = streamVariantId
      url = "http://" <> addr <> ":3000/api/agents/ingestAggregator/" <> streamId <> "/activeIngests/" <> variantId
    restResult <- SpudGun.post url (JSON.writeJSON thisNode)
    case restResult of
      -- TODO - retry on timer?
      Left _ -> pure $ unit
      Right _ -> pure $ unit


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
      -- TODO - retry on timer?
      pure $ candidate
    Just (ServerAddress addr) -> do
      let
        -- TODO - functions to make URLs from ServerAddress
        url = "http://" <> addr <> ":3000/api/agents/ingestAggregator"
      restResult <- SpudGun.post url (JSON.writeJSON streamDetails)
      case restResult of
        -- TODO - retry on timer?
        Left _ -> pure $ candidate
        Right _ -> pure $ candidate

logInfo :: forall a. String -> a -> Effect Foreign
logInfo msg metaData = Logger.info msg (Record.merge { domain: ((atom (show Agent.Ingest)) : nil) } { misc: metaData })
