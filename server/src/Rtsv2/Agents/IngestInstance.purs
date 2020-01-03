module Rtsv2.Agents.IngestInstance
  ( startLink
  , isActive
  , stopIngest
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Debug.Trace (spy)
import Effect (Effect)
import Erl.Atom (atom)
import Erl.Data.List (nil, (:))
import Erl.Data.Tuple (tuple2, tuple3)
import Erl.ModuleName (NativeModuleName(..))
import Foreign (Foreign, unsafeToForeign)
import Gproc as Gproc
import Logger as Logger
import Pinto (ServerName(..), StartLinkResult)
import Pinto.Gen (CallResult(..))
import Pinto.Gen as Gen
import Record as Record
import Rtsv2.Audit as Audit
import Rtsv2.Agents.IngestAggregatorInstance as IngestAggregatorInstance
import Rtsv2.Agents.IngestAggregatorInstanceSup as IngestAggregatorInstanceSup
import Rtsv2.Agents.IntraPoP as IntraPoP
import Rtsv2.Load as Load
import Rtsv2.PoPDefinition as PoPDefinition
import Shared.Agent as Agent
import Shared.Stream (StreamVariantId, toStreamId)
import Shared.Types (ServerAddress)

type State
  = { }

isActive :: StreamVariantId -> Effect Boolean
isActive streamVariantId = Gproc.isRegistered (tuple2 "ingest" streamVariantId)

serverName :: StreamVariantId -> ServerName State Unit
serverName streamVariantId = Via (NativeModuleName $ atom "gproc") $ unsafeToForeign (tuple3 (atom "n") (atom "l") (tuple2 "ingest" streamVariantId))

startLink :: StreamVariantId -> Effect StartLinkResult
startLink streamVariantId = Gen.startLink (serverName streamVariantId) (init streamVariantId) Gen.defaultHandleInfo

stopIngest :: StreamVariantId -> Effect Unit
stopIngest streamVariantId =
  Gen.doCall (serverName streamVariantId) \state -> do
    -- TODO - single ingest can't stop the aggregator
    _ <- IngestAggregatorInstance.stopAggregator (toStreamId streamVariantId)
    _ <- Audit.ingestStop streamVariantId
    pure $ CallStop unit state

init :: StreamVariantId -> Effect State
init streamVariantId = do
  _ <- logInfo "Ingest starting" {streamVariantId: streamVariantId}
  _ <- Audit.ingestStart streamVariantId
  maybeAggregator <- getAggregator streamVariantId
  pure {}

getAggregator :: StreamVariantId -> Effect (Maybe ServerAddress)
getAggregator streamVariantId = do
  maybeAggregator <- IntraPoP.whereIsIngestAggregator streamVariantId
  case maybeAggregator of
    Just aggregator ->
      pure maybeAggregator
    Nothing ->
      launchLocalOrRemote streamVariantId

launchLocalOrRemote :: StreamVariantId -> Effect (Maybe ServerAddress)
launchLocalOrRemote streamVariantId = do
  currentLoad <- Load.load
  if
    currentLoad < (wrap 50.0) then do
      _ <- IngestAggregatorInstanceSup.startAggregator (toStreamId streamVariantId)
      Just <$> PoPDefinition.thisNode
    else
      launchRemote streamVariantId

launchRemote :: StreamVariantId -> Effect (Maybe ServerAddress)
launchRemote streamVariantId = do
  IntraPoP.getIdleServer

-- check for existing aggregator - if one, talk to it
-- if none, then check our load.  If low enough, start one here
-- if load too high, pick two random servers and ask the best to start one
-- if start fails, repeat

logInfo :: forall a. String -> a -> Effect Foreign
logInfo msg metaData = Logger.info msg (Record.merge { domain: ((atom (show Agent.Ingest)) : nil) } { misc: metaData })
