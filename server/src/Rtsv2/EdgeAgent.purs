module Rtsv2.EdgeAgent
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
import Rtsv2.IntraPoPAgent as IntraPoPAgent
import Shared.Agent as Agent
import Shared.Stream (StreamId(..), StreamVariantId, toStreamId)

type State
  = { }

serverName :: StreamId -> ServerName State Unit
serverName streamId = Via (NativeModuleName $ atom "gproc") $ unsafeToForeign (tuple3 (atom "n") (atom "l") (tuple2 "edge" streamId))

startLink :: StreamId -> Effect StartLinkResult
startLink streamId = Gen.startLink (serverName streamId) (init streamId) Gen.defaultHandleInfo

init :: StreamId -> Effect State
init streamId = do
  _ <- logInfo "Edge starting" {streamId: streamId}

  -- maybeAggregator <- IntraPoPAgent.whereIsIngestAggregator streamVariantId
  -- case maybeAggregator of
  --   Just relay ->
  --     pure {}
  --   Nothing -> do
  --     -- Launch
  --     _ <- IngestAggregatorAgentSup.startAggregator (toStreamId streamVariantId)
  pure {}

logInfo :: forall a. String -> a -> Effect Foreign
logInfo msg metaData = Logger.info msg (Record.merge { domain: ((atom (show Agent.Edge)) : nil) } { misc: metaData })
