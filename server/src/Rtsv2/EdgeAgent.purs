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
import Gproc (isRegistered)
import Logger as Logger
import Pinto (ServerName(..), StartLinkResult)
import Pinto.Gen as Gen
import Record as Record
import Rtsv2.IntraPoPAgent as IntraPoPAgent
import Rtsv2.StreamRelayAgentSup as StreamRelayAgentSup
import Shared.Agent as Agent
import Shared.Stream (StreamId)

type State
  = { }

gprocName :: StreamId -> Foreign
gprocName streamId = unsafeToForeign (tuple3 (atom "n") (atom "l") (tuple2 "edge" streamId))

serverName :: StreamId -> ServerName State Unit
serverName streamId = Via (NativeModuleName $ atom "gproc") $ gprocName streamId

startLink :: StreamId -> Effect StartLinkResult
startLink streamId = Gen.startLink (serverName streamId) (init streamId) Gen.defaultHandleInfo

init :: StreamId -> Effect State
init streamId = do
  _ <- logInfo "Edge starting" {streamId: streamId}
  _ <- IntraPoPAgent.announceEdgeIsActive streamId
  maybeRelay <- IntraPoPAgent.whereIsStreamRelay streamId
  case maybeRelay of
    Just relay ->
      pure {}
    Nothing -> do
      -- Launch
      _ <- StreamRelayAgentSup.startRelay streamId
      pure {}

logInfo :: forall a. String -> a -> Effect Foreign
logInfo msg metaData = Logger.info msg (Record.merge { domain: ((atom (show Agent.Edge)) : nil) } { misc: metaData })
