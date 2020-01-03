module Rtsv2.Agents.StreamRelayInstance
  ( startLink
  , init
  ) where

import Prelude

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
import Shared.Agent as Agent
import Shared.Stream (StreamId)

type State
  = { }

serverName :: StreamId -> ServerName State Unit
serverName streamId = Via (NativeModuleName $ atom "gproc") $ unsafeToForeign (tuple3 (atom "n") (atom "l") (tuple2 "streamRelay" streamId))

startLink :: StreamId -> Effect StartLinkResult
startLink streamId = Gen.startLink (serverName streamId) (init streamId) Gen.defaultHandleInfo

init :: StreamId -> Effect State
init streamId = do
  _ <- logInfo "StreamRelay starting" {streamId: streamId}
  pure {}

logInfo :: forall a. String -> a -> Effect Foreign
logInfo msg metaData = Logger.info msg (Record.merge { domain: ((atom (show Agent.StreamRelay)) : nil) } { misc: metaData })
