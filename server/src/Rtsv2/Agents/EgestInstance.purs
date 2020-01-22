module Rtsv2.Agents.EgestInstance
  ( startLink
  , isActive
  , addClient
  , removeClient
  , currentStats
  ) where

import Prelude

import Bus as Bus
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap, wrap)
import Effect (Effect)
import Erl.Atom (Atom, atom)
import Erl.Data.List (List, singleton)
import Erl.Utils (Milliseconds, Ref, makeRef)
import Logger (Logger)
import Logger as Logger
import Pinto (ServerName, StartLinkResult)
import Pinto.Gen (CallResult(..), CastResult(..))
import Pinto.Gen as Gen
import Pinto.Timer as Timer
import Rtsv2.Agents.IntraPoP (IntraPoPBusMessage(..))
import Rtsv2.Agents.IntraPoP as IntraPoP
import Rtsv2.Agents.StreamRelayInstanceSup as StreamRelayInstanceSup
import Rtsv2.Config as Config
import Rtsv2.Names as Names
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
         | IntraPoPBus IntraPoP.IntraPoPBusMessage

serverName :: StreamId -> ServerName State Msg
serverName streamId = Names.egestInstanceName streamId

isActive :: StreamId -> Effect Boolean
isActive streamId = Names.isRegistered (serverName streamId)

startLink :: StreamId -> Effect StartLinkResult
startLink streamId = Gen.startLink (serverName streamId) (init streamId) handleInfo

addClient :: StreamId -> Effect Unit
addClient streamId = Gen.doCall (serverName streamId) doAddClient

doAddClient :: State -> Effect (CallResult Unit State)
doAddClient state@{clientCount} = do
  _ <- logInfo "Add client" {newCount: clientCount + 1}
  pure $ CallReply unit state{ clientCount = clientCount + 1
                             , stopRef = Nothing
                             }

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
  pure $ CallReply unit state{ clientCount = 0
                             , stopRef = Just makeRef
                             }

doRemoveClient state@{clientCount} = do
  _ <- logInfo "Remove client" { newCount: clientCount - 1 }
  pure $ CallReply unit state{ clientCount = clientCount - 1 }

currentStats :: StreamId -> Effect Int
currentStats streamId =
  Gen.call (serverName streamId) \state@{clientCount} ->
    CallReply clientCount state

init :: StreamId -> Effect State
init streamId = do
  {egestAvailableAnnounceMs, lingerTimeMs} <- Config.egestAgentConfig
  _ <- logInfo "Egest starting" {streamId: streamId}
  _ <- Bus.subscribe (serverName streamId) IntraPoP.bus IntraPoPBus
  _ <- IntraPoP.announceEgestIsAvailable streamId
  _ <- Timer.sendEvery (serverName streamId) egestAvailableAnnounceMs Tick
  maybeRelay <- IntraPoP.whereIsStreamRelay streamId
  let
    state ={ streamId
           , clientCount: 0
           , lingerTime: wrap lingerTimeMs
           , stopRef: Nothing
           }
  case maybeRelay of
    Just relay ->
      pure state
    Nothing -> do
      -- Launch
--      _ <- StreamRelayInstanceSup.startRelay streamId
      pure state

handleInfo :: Msg -> State -> Effect (CastResult State)
handleInfo msg state@{streamId} =
  case msg of
    Tick -> CastNoReply <$> handleTick state

    MaybeStop ref -> maybeStop ref state

    IntraPoPBus (IngestAggregatorExited stoppingStreamId serverAddress)
      | stoppingStreamId == streamId -> pure $ CastStop state
      | otherwise -> pure $ CastNoReply state


handleTick :: State -> Effect State
handleTick state@{streamId} = do
  _ <- IntraPoP.announceEgestIsAvailable streamId
  pure state

maybeStop :: Ref -> State -> Effect (CastResult State)
maybeStop ref state@{streamId
                    , clientCount
                    , stopRef}
  | (clientCount == 0) && (Just ref == stopRef) = do
    _ <- logInfo "Egest stopping" {streamId: streamId}
    _ <- IntraPoP.announceEgestStopped streamId
    pure $ CastStop state

  | otherwise = pure $ CastNoReply state


--------------------------------------------------------------------------------
-- Log helpers
--------------------------------------------------------------------------------
domains :: List Atom
domains = Agent.Egest # show # atom # singleton

logInfo :: forall a. Logger a
logInfo = domainLog Logger.info

-- logWarning :: forall a. Logger a
-- logWarning = domainLog Logger.warning

domainLog :: forall a. Logger {domain :: List Atom, misc :: a} -> Logger a
domainLog = Logger.doLog domains
