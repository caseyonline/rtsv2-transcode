module Rtsv2.Agents.IngestInstance
  ( startLink
  , isActive
  , stopIngest
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Effect (Effect)
import Erl.Atom (Atom, atom)
import Erl.Data.List (List, nil, (:))
import Logger (Logger)
import Logger as Logger
import Pinto (ServerName, StartLinkResult)
import Pinto.Gen (CallResult(..))
import Pinto.Gen as Gen
import Rtsv2.Agents.IngestAggregatorInstance as IngestAggregatorInstance
import Rtsv2.Agents.IngestAggregatorInstanceSup as IngestAggregatorInstanceSup
import Rtsv2.Agents.IntraPoP as IntraPoP
import Rtsv2.Audit as Audit
import Rtsv2.Load as Load
import Rtsv2.Names as Names
import Rtsv2.PoPDefinition as PoPDefinition
import Shared.Agent as Agent
import Shared.Stream (StreamAndVariant(..), toStreamId)
import Shared.Types (LocatedServer(..), ServerAddress, ServerLoad(..), locatedServerAddress)

type State
  = { }

isActive :: StreamAndVariant -> Effect Boolean
isActive streamAndVariant = Names.isRegistered (serverName streamAndVariant)

serverName :: StreamAndVariant -> ServerName State Unit
serverName = Names.ingestInstanceName

startLink :: StreamAndVariant -> Effect StartLinkResult
startLink streamAndVariant = Gen.startLink (serverName streamAndVariant) (init streamAndVariant) Gen.defaultHandleInfo

stopIngest :: StreamAndVariant -> Effect Unit
stopIngest streamAndVariant =
  Gen.doCall (serverName streamAndVariant) \state -> do
    -- TODO - single ingest can't stop the aggregator
    _ <- IngestAggregatorInstance.stopAggregator (toStreamId streamAndVariant)
    _ <- Audit.ingestStop streamAndVariant
    pure $ CallStop unit state

init :: StreamAndVariant -> Effect State
init streamAndVariant = do
  _ <- logInfo "Ingest starting" {streamAndVariant: streamAndVariant}
  thisNode <- PoPDefinition.thisNode
  _ <- Audit.ingestStart streamAndVariant
  maybeAggregator <- getAggregator streamAndVariant
  _ <- addVariant thisNode streamAndVariant maybeAggregator
  pure {}

addVariant :: ServerAddress -> StreamAndVariant -> Maybe ServerAddress -> Effect Unit
addVariant thisNode streamAndVariant aggregatorAddress
  | aggregatorAddress == Just thisNode = do
    _ <- IngestAggregatorInstance.addVariant streamAndVariant
    pure unit
  | otherwise = pure unit -- TODO - HTTP call...

getAggregator :: StreamAndVariant -> Effect (Maybe ServerAddress)
getAggregator streamAndVariant@(StreamAndVariant streamId _variantId)  = do
  maybeAggregator <- IntraPoP.whereIsIngestAggregator streamId
  case maybeAggregator of
    Just (LocatedServer serverAddress _serverLocation) ->
      pure $ Just serverAddress
    Nothing ->
      launchLocalOrRemote streamAndVariant

launchLocalOrRemote :: StreamAndVariant -> Effect (Maybe ServerAddress)
launchLocalOrRemote streamAndVariant@(StreamAndVariant streamId _variantId) = do
  currentLoad <- Load.load
  if
    currentLoad < (wrap 50.0) then do
      _ <- IngestAggregatorInstanceSup.startAggregator streamId
      Just <$> PoPDefinition.thisNode
    else
      launchRemote streamAndVariant

launchRemote :: StreamAndVariant -> Effect (Maybe ServerAddress)
launchRemote streamAndVariant =
  -- TODO - need to make http call to idle server to request it starts an aggregator, and then retry if it returns no
  IntraPoP.getIdleServer predicate <#> (map locatedServerAddress)
  where
    predicate (ServerLoad _locatedServer load) = (load < wrap 70.2231222)

--------------------------------------------------------------------------------
-- Log helpers
--------------------------------------------------------------------------------
domains :: List Atom
domains = atom <$> (show Agent.Ingest :  "Instance" : nil)

logInfo :: forall a. Logger a
logInfo = domainLog Logger.info

--logWarning :: forall a. Logger a
--logWarning = domainLog Logger.warning

domainLog :: forall a. Logger {domain :: List Atom, misc :: a} -> Logger a
domainLog = Logger.doLog domains
