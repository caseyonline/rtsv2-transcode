module Rtsv2.Agents.StreamRelayInstance
  ( startLink
  , isAvailable
  , init
  , status
  , Status
  ) where

import Prelude

import Effect (Effect)
import Erl.Atom (Atom, atom)
import Erl.Data.List (List, nil, (:))
import Erl.Data.Tuple (tuple2, tuple3)
import Erl.ModuleName (NativeModuleName(..))
import Foreign (unsafeToForeign)
import Logger (Logger)
import Logger as Logger
import Pinto (ServerName(..), StartLinkResult)
import Pinto.Gen (CallResult(..))
import Pinto.Gen as Gen
import Rtsv2.Names as Names
import Shared.Agent as Agent
import Shared.Stream (StreamId)


type Status = {}
type State
  = { status :: Status }

serverName :: StreamId -> ServerName State Unit
serverName streamId = Via (NativeModuleName $ atom "gproc") $ unsafeToForeign (tuple3 (atom "n") (atom "l") (tuple2 "streamRelay" streamId))

startLink :: StreamId -> Effect StartLinkResult
startLink streamId = Gen.startLink (serverName streamId) (init streamId) Gen.defaultHandleInfo

isAvailable :: StreamId -> Effect Boolean
isAvailable streamId = Names.isRegistered (serverName streamId)

status  :: StreamId -> Effect Status
status streamId =
  exposeStateMember _.status streamId


init :: StreamId -> Effect State
init streamId = do
  _ <- logInfo "StreamRelay starting" {streamId: streamId}
  pure {status: {}}


exposeStateMember :: forall a. (State -> a) -> StreamId -> Effect a
exposeStateMember member streamId = Gen.doCall (serverName streamId)
  \state -> pure $ CallReply (member state) state

--------------------------------------------------------------------------------
-- Log helpers
--------------------------------------------------------------------------------
domains :: List Atom
domains = atom <$> (show Agent.StreamRelay :  "Instance" : nil)

logInfo :: forall a. Logger a
logInfo = domainLog Logger.info

--logWarning :: forall a. Logger a
--logWarning = domainLog Logger.warning

domainLog :: forall a. Logger {domain :: List Atom, misc :: a} -> Logger a
domainLog = Logger.doLog domains
