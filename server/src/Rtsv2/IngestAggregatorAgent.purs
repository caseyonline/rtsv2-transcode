module Rtsv2.IngestAggregatorAgent
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
import Rtsv2.IntraPoPAgent (streamIsAvailable)
import Shared.Stream (StreamId)

type Config
  = { streamId :: StreamId }

type State
  = { config :: Config
    }

serverName :: StreamId -> ServerName State Unit
serverName s = Via (NativeModuleName $ atom "gproc") $ unsafeToForeign (tuple3 (atom "n") (atom "l") (tuple2 "ingest" s))

--nativeName (Via (NativeModuleName m) name) = unsafeCoerce $ tuple3 (atom "via") m name
startLink :: Config -> Effect StartLinkResult
startLink args = Gen.startLink (serverName args.streamId) (init args) Gen.defaultHandleInfo

init :: Config -> Effect State
init config = do
  _ <- streamIsAvailable config.streamId

  pure
        { config
        }
 -- TODO - what if we want to have the relay run remotely (load etc) -- Find / create if there is a relay for this stream -- ask intrapopstate --
