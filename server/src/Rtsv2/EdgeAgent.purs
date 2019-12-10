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
import Pinto.Timer as Timer
import Record as Record
import Rtsv2.Config as Config
import Rtsv2.IntraPoPAgent as IntraPoPAgent
import Rtsv2.StreamRelayAgentSup as StreamRelayAgentSup
import Shared.Agent as Agent
import Shared.Stream (StreamId)

type State
  = { streamId :: StreamId
    , clientCount :: Int }

data Msg
  = Tick

isActive :: StreamId -> Effect Boolean
isActive streamId = Gproc.isRegistered (tuple2 "edge" streamId)

startLink :: StreamId -> Effect StartLinkResult
startLink streamId = Gen.startLink (serverName streamId) (init streamId) handleInfo

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

serverName :: StreamId -> ServerName State Msg
serverName streamId = Via (NativeModuleName $ atom "gproc") $ gprocName streamId

init :: StreamId -> Effect State
init streamId = do
  {edgeAvailableAnnounceMs} <- Config.edgeAgentConfig
  _ <- logInfo "Edge starting" {streamId: streamId}
  _ <- IntraPoPAgent.announceEdgeIsActive streamId
  _ <- Timer.sendEvery (serverName streamId) edgeAvailableAnnounceMs Tick
  maybeRelay <- IntraPoPAgent.whereIsStreamRelay streamId
  case maybeRelay of
    Just relay ->
      pure { streamId
           , clientCount : 0}
    Nothing -> do
      -- Launch
      _ <- StreamRelayAgentSup.startRelay streamId
      pure { streamId
           , clientCount : 0}

handleInfo :: Msg -> State -> Effect State
handleInfo msg state =
  case msg of
    Tick -> handleTick state

handleTick :: State -> Effect State
handleTick state@{streamId} = do
  _ <- logInfo "announce" {streamId}
  _ <- IntraPoPAgent.announceEdgeIsActive streamId
  pure state

logInfo :: forall a. String -> a -> Effect Foreign
logInfo msg metaData = Logger.info msg (Record.merge { domain: ((atom (show Agent.Edge)) : nil) } { misc: metaData })
