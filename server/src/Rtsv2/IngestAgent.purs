module Rtsv2.IngestAgent
  ( startLink
  , init
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Erl.Atom (atom)
import Erl.Data.List (nil, (:))
import Erl.Data.Tuple (tuple2, tuple3)
import Erl.ModuleName (NativeModuleName(..))
import Foreign (Foreign, unsafeToForeign)
import Logger as Logger
import Pinto (ServerName(..), StartLinkResult)
import Pinto.Gen as Gen
import Record as Record
import Rtsv2.Audit as Audit
import Rtsv2.IngestAggregatorAgentSup as IngestAggregatorAgentSup
import Rtsv2.IntraPoPAgent as IntraPoPAgent
import Shared.Agent as Agent
import Shared.Stream (StreamVariantId, toStreamId)

type State
  = { }

serverName :: StreamVariantId -> ServerName State Unit
serverName sv = Via (NativeModuleName $ atom "gproc") $ unsafeToForeign (tuple3 (atom "n") (atom "l") (tuple2 "ingest" sv))

startLink :: StreamVariantId -> Effect StartLinkResult
startLink streamVariantId = Gen.startLink (serverName streamVariantId) (init streamVariantId) Gen.defaultHandleInfo

init :: StreamVariantId -> Effect State
init streamVariantId = do
  _ <- logInfo "Ingest starting" {streamVariantId: streamVariantId}
  _ <- Audit.ingestStart streamVariantId
  maybeAggregator <- IntraPoPAgent.whereIsIngestAggregator streamVariantId
  case maybeAggregator of
    Just relay ->
      pure {}
    Nothing -> do
      -- Launch
      _ <- IngestAggregatorAgentSup.startAggregator (toStreamId streamVariantId)
      pure {}


logInfo :: forall a. String -> a -> Effect Foreign
logInfo msg metaData = Logger.info msg (Record.merge { domain: ((atom (show Agent.Ingest)) : nil) } { misc: metaData })
