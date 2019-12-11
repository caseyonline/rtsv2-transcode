module Rtsv2.EdgeAgent
  ( startLink
  , isActive
  , addClient
  , removeClient
  , currentClientCount
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap, wrap)
import Effect (Effect)
import Erl.Atom (atom)
import Erl.Data.List (nil, (:))
import Erl.Data.Tuple (tuple2, tuple3)
import Erl.ModuleName (NativeModuleName(..))
import Erl.Utils (Milliseconds, Ref, makeRef)
import Foreign (Foreign, unsafeToForeign)
import Gproc as Gproc
import Logger as Logger
import Pinto (ServerName(..), StartLinkResult)
import Pinto.Gen (CallResult(..), CastResult(..))
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
    , clientCount :: Int
    , lingerTime :: Milliseconds
    , stopRef :: Maybe Ref
    }

data Msg = Tick
         | MaybeStop Ref

isActive :: StreamId -> Effect Boolean
isActive streamId = Gproc.isRegistered (tuple2 "edge" streamId)

startLink :: StreamId -> Effect StartLinkResult
startLink streamId = Gen.startLink (serverName streamId) (init streamId) handleInfo

addClient :: StreamId -> Effect Unit
addClient streamId = Gen.doCall (serverName streamId) doAddClient

doAddClient :: State -> Effect (CallResult Unit State)
doAddClient state@{clientCount} = do
  _ <- logInfo "Add client" {newCount: clientCount + 1}
  pure $ CallReply unit state{clientCount = clientCount + 1,
                              stopRef = Nothing}

removeClient :: StreamId -> Effect Unit
removeClient streamId = Gen.doCall (serverName streamId) doRemoveClient

doRemoveClient :: State -> Effect (CallResult Unit State)
doRemoveClient state@{clientCount: 0} = do
  _ <- logInfo "Remove client - already zero" {}
  pure $ CallReply unit state
doRemoveClient state@{clientCount: 1, lingerTime, streamId} = do
  let
    ref = makeRef
  _ <- logInfo "Last client gone, start stop timer" {}
  _ <- Timer.sendAfter (serverName streamId) (unwrap lingerTime) (MaybeStop ref)
  pure $ CallReply unit state{clientCount = 0,
                              stopRef = Just makeRef}
    
doRemoveClient state@{clientCount} = do
  _ <- logInfo "Remove client" {newCount: clientCount - 1}
  pure $ CallReply unit state{clientCount = clientCount - 1}
    
currentClientCount :: StreamId -> Effect Int
currentClientCount streamId =
  Gen.call (serverName streamId) \state@{clientCount} ->
    CallReply clientCount state

stop :: StreamId -> Effect Unit
stop streamId =
  Gen.cast (serverName streamId) \state ->
    CastStop state

gprocName :: StreamId -> Foreign
gprocName streamId = unsafeToForeign (tuple3 (atom "n") (atom "l") (tuple2 "edge" streamId))

serverName :: StreamId -> ServerName State Msg
serverName streamId = Via (NativeModuleName $ atom "gproc") $ gprocName streamId

init :: StreamId -> Effect State
init streamId = do
  {edgeAvailableAnnounceMs, lingerTimeMs} <- Config.edgeAgentConfig
  _ <- logInfo "Edge starting" {streamId: streamId}
  _ <- IntraPoPAgent.announceEdgeIsActive streamId
  _ <- Timer.sendEvery (serverName streamId) edgeAvailableAnnounceMs Tick
  maybeRelay <- IntraPoPAgent.whereIsStreamRelay streamId
  let 
    state ={ streamId
           , clientCount: 0
           , lingerTime: wrap lingerTimeMs
          , stopRef: Nothing}
  case maybeRelay of
    Just relay ->
      pure state
    Nothing -> do
      -- Launch
      _ <- StreamRelayAgentSup.startRelay streamId
      pure state

handleInfo :: Msg -> State -> Effect State
handleInfo msg state =
  case msg of
    Tick -> handleTick state

    MaybeStop ref -> maybeStop ref state

handleTick :: State -> Effect State
handleTick state@{streamId} = do
  _ <- IntraPoPAgent.announceEdgeIsActive streamId
  pure state

maybeStop :: Ref -> State -> Effect State
maybeStop ref state@{streamId
                    , clientCount
                    , stopRef}
  | (clientCount == 0) && (Just ref == stopRef) = do
    _ <- logInfo "Edge stopping" {streamId: streamId}
    _ <- IntraPoPAgent.announceEdgeStopped streamId
    _ <- stop streamId
    pure state

  | otherwise = pure state

logInfo :: forall a. String -> a -> Effect Foreign
logInfo msg metaData = Logger.info msg (Record.merge { domain: ((atom (show Agent.Edge)) : nil) } { misc: metaData })
