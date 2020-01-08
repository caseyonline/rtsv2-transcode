module Rtsv2.Agents.IngestInstance
  ( startLink
  , isActive
  , stopIngest
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Erl.Atom (atom)
import Erl.Data.List (nil, (:))
import Foreign (Foreign)
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
import Shared.Stream (StreamVariantId, toStreamId)
import Shared.Types (ServerAddress)

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
    -- TODO - single ingest can't stop the aggregator
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
addVariant thisNode streamVariantId aggregatorAddress
  | aggregatorAddress == Just thisNode = do
    _ <- IngestAggregatorInstance.addVariant streamVariantId
    pure unit
  | otherwise = pure unit -- TODO - HTTP call...

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
  -- TODO - need to make http call to idle server to request it starts an aggregator, and then retry if it returns no
  IntraPoP.getIdleServer

logInfo :: forall a. String -> a -> Effect Foreign
logInfo msg metaData = Logger.info msg (Record.merge { domain: ((atom (show Agent.Ingest)) : nil) } { misc: metaData })
