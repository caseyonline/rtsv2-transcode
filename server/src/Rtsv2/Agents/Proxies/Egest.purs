module Rtsv2.Agents.Proxies.Egest
       ( createProxyForExisting
       , createProxyForNew
       , whereIsLocal
       , whereIsRemote
       , foo
       , startLink
       )
       where

import Prelude

import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (wrap)
import Debug.Trace (spy)
import Effect (Effect)
import Erl.Atom (Atom, atom)
import Erl.Data.List (List, nil, (:))
import Gproc as Gproc
import Logger (Logger)
import Logger as Logger
import Pinto (ServerName, StartLinkResult)
import Pinto.Gen (CastResult(..))
import Pinto.Gen as Gen
import Rtsv2.Names as Names
import Rtsv2.PoPDefinition as PoPDefinition
import Shared.Agent as Agent
import Shared.Stream (StreamId(..))
import Shared.Types (Server(..), ServerAddress(..))




type State = {}
type Msg = Unit

whereIsRemote :: StreamId -> Effect (List (ServerName State Msg))
whereIsRemote streamId = do
  Gproc.match $ Names.egestRemoteProxyMatch streamId


whereIsLocal :: StreamId -> Effect (Maybe (ServerName State Msg))
whereIsLocal streamId = do
  let name = Names.egestLocalProxyName streamId
  isPresentLocally <- Gproc.isRegistered name
  if isPresentLocally
  then pure $ Just name
  else pure $ Nothing




createProxyForExisting = unit
createProxyForNew = unit



--foo :: Effect Unit
foo = do
  startLink unit



startLink :: Unit -> Effect StartLinkResult
startLink args = do
  thisServer <- PoPDefinition.getThisServer
  let (Server foo) = thisServer
      thatServer = (Server foo{address = wrap "server2"})
      _ = spy "name1" $ remoteServerName (wrap "slot1") thisServer
      _ = spy "name2" $ remoteServerName (wrap "slot1") thatServer

  _ <- Gen.startLink (remoteServerName (wrap "slot1") $ spy "this" thisServer) (init args) handleInfo
  Gen.startLink (remoteServerName (wrap "slot1") $ spy "that" thatServer) (init args) handleInfo


init :: Unit -> Effect State
init args = do
  _ <- logInfo "Egest proxy starting" {}
  pure $ {}

handleInfo :: Msg -> State -> Effect (CastResult State)
handleInfo msg state =
  pure $ CastNoReply state




remoteServerName :: StreamId -> Server -> ServerName State Msg
remoteServerName streamId server =
  Names.egestRemoteProxyName streamId server


--findEgestForStream :: StreamId -> Effect (Either FailureReason EgestLocation)
--findEgestForStream streamId =



--------------------------------------------------------------------------------
-- Log helpers
--------------------------------------------------------------------------------
domains :: List Atom
domains = atom (show Agent.Egest) : atom "proxy" : nil

logInfo :: forall a. Logger a
logInfo = domainLog Logger.info

logWarning :: forall a. Logger a
logWarning = domainLog Logger.warning

domainLog :: forall a. Logger {domain :: List Atom, misc :: a} -> Logger a
domainLog = Logger.doLog domains
