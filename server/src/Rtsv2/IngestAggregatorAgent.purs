module Rtsv2.IngestAggregatorAgent
  ( startLink
  , init
  ) where

import Prelude

import Effect (Effect)
import Erl.Atom (atom)
import Erl.Data.Tuple (tuple2, tuple3)
import Erl.ModuleName (NativeModuleName(..))
import Foreign (unsafeToForeign)
import Pinto (ServerName(..), StartLinkResult)
import Pinto.Gen as Gen
import Rtsv2.Config as Config
import Pinto.Timer as Timer
import Rtsv2.IntraPoPAgent (announceStreamIsAvailable)
import Shared.Stream (StreamId)

type State
  = { config :: Config.IngestAggregatorAgentConfig
    , streamId :: StreamId
    }

data Msg
  = Tick

serverName :: StreamId -> ServerName State Msg
serverName s = Via (NativeModuleName $ atom "gproc") $ unsafeToForeign (tuple3 (atom "n") (atom "l") (tuple2 "ingest" s))

startLink :: StreamId -> Effect StartLinkResult
startLink streamId = Gen.startLink (serverName streamId) (init streamId) handleInfo

init :: StreamId -> Effect State
init streamId = do
  let
    streamAvailableAnnounceMs = 1000 -- TODO - should be in config - goes circular
  _ <- Config.ingestAggregatorAgentConfig
  _ <- Timer.sendEvery (serverName streamId) streamAvailableAnnounceMs Tick
  _ <- announceStreamIsAvailable streamId

  pure
        { config : {streamAvailableAnnounceMs} --config
        , streamId
        }

handleInfo :: Msg -> State -> Effect State
handleInfo msg state =
  case msg of
    Tick -> handleTick state

handleTick :: State -> Effect State
handleTick state@{streamId} = do
  _ <- announceStreamIsAvailable streamId
  pure state

-- TODO - what if we want to have the relay run remotely (load etc) -- Find / create if there is a relay for this stream -- ask intrapopstate --
