module Rtsv2.IngestAgent
       ( startLink
       , init
       , Config
       ) where

import Prelude

import Effect (Effect)
import Erl.Atom (atom)
import Erl.Data.Tuple (tuple2, tuple3)
import Erl.ModuleName (NativeModuleName(..))
import Foreign (unsafeToForeign)
import Pinto (ServerName(..), StartLinkResult)
import Pinto.Gen as Gen
import Shared.Stream (StreamVariantId)

type Config
  = { streamVariantId :: StreamVariantId}

type State
  = { config :: Config
    }


serverName :: StreamVariantId -> ServerName State Unit
serverName sv = Via (NativeModuleName $ atom "gproc") $ unsafeToForeign (tuple3 (atom "n") (atom "l") (tuple2 "ingest" sv))
--nativeName (Via (NativeModuleName m) name) = unsafeCoerce $ tuple3 (atom "via") m name

startLink :: Config  -> Effect StartLinkResult
startLink args = Gen.startLink (serverName args.streamVariantId) (init args) Gen.defaultHandleInfo

init :: Config -> Effect State
init config = do
  pure { config
       }
