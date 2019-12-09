module Rtsv2.EdgeAgent
  ( startLink
  , isActive
  , addClient
  , currentClientCount
  ) where

import Prelude

import Data.Maybe (Maybe(..))
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
import Rtsv2.IntraPoPAgent as IntraPoPAgent
import Rtsv2.StreamRelayAgentSup as StreamRelayAgentSup
import Shared.Agent as Agent
import Shared.Stream (StreamId)

type State
  = { clientCount :: Int }

isActive :: StreamId -> Effect Boolean
isActive streamId = Gproc.isRegistered (tuple2 "edge" streamId)

startLink :: StreamId -> Effect StartLinkResult
startLink streamId = Gen.startLink (serverName streamId) (init streamId) Gen.defaultHandleInfo

addClient :: StreamId -> Effect Unit
addClient streamId =
  Gen.call (serverName streamId) \state@{clientCount} ->
    CallReply unit state{clientCount = clientCount + 1}

currentClientCount :: StreamId -> Effect Int
currentClientCount streamId =
  Gen.call (serverName streamId) \state@{clientCount} ->
    CallReply clientCount state

gprocName :: StreamId -> Foreign
gprocName streamId = unsafeToForeign (tuple3 (atom "n") (atom "l") (tuple2 "edge" streamId))

serverName :: StreamId -> ServerName State Unit
serverName streamId = Via (NativeModuleName $ atom "gproc") $ gprocName streamId

init :: StreamId -> Effect State
init streamId = do
  _ <- logInfo "Edge starting" {streamId: streamId}
  _ <- IntraPoPAgent.announceEdgeIsActive streamId
  maybeRelay <- IntraPoPAgent.whereIsStreamRelay streamId
  case maybeRelay of
    Just relay ->
      pure {clientCount : 0}
    Nothing -> do
      -- Launch
      _ <- StreamRelayAgentSup.startRelay streamId
      pure {clientCount : 0}

logInfo :: forall a. String -> a -> Effect Foreign
logInfo msg metaData = Logger.info msg (Record.merge { domain: ((atom (show Agent.Edge)) : nil) } { misc: metaData })
