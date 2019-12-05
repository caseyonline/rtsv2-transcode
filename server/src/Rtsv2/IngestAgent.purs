module Rtsv2.IngestAgent
  ( startLink
  , init
  , Config
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Erl.Atom (atom)
import Erl.Data.Tuple (tuple2, tuple3)
import Erl.ModuleName (NativeModuleName(..))
import Foreign (unsafeToForeign)
import Pinto (ServerName(..), StartLinkResult)
import Pinto.Gen as Gen
import Rtsv2.IngestAggregatorAgentSup as IngestAggregatorAgentSup
import Rtsv2.IntraPoPAgent as IntraPoPAgent
import Shared.Stream (StreamVariantId, toStreamId)

type Config
  = { }

type State
  = { config :: Config
    }

serverName :: StreamVariantId -> ServerName State Unit
serverName sv = Via (NativeModuleName $ atom "gproc") $ unsafeToForeign (tuple3 (atom "n") (atom "l") (tuple2 "ingest" sv))

startLink :: StreamVariantId -> Effect StartLinkResult
startLink streamVariantId = Gen.startLink (serverName streamVariantId) (init streamVariantId) Gen.defaultHandleInfo

init :: StreamVariantId -> Effect State
init streamVariantId = do
  maybeAggregator <- IntraPoPAgent.whereIsIngestAggregator streamVariantId
  case maybeAggregator of
    Just relay ->
      pure
        { config: {}
        }
    Nothing -> do
      -- Launch
      _ <- IngestAggregatorAgentSup.startAggregator (toStreamId streamVariantId) 
      pure
        { config: {}
        }
 -- TODO - what if we want to have the relay run remotely (load etc) -- Find / create if there is a relay for this stream -- ask intrapopstate --
