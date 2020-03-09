module Rtsv2.Agents.PersistentInstanceState
       ( startLink
       , Msg
       , StartArgs
       , State
       , StateServerName
       , getInstanceData
       , recordInstanceData
       ) where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Erl.Atom (Atom)
import Erl.Data.List (List)
import Erl.Process.Raw (Pid)
import Erl.Utils (ExitMessage(..), ExitReason(..))
import Erl.Utils as Erl
import Foreign (Foreign)
import Logger as Logger
import Pinto (ServerName, StartLinkResult(..))
import Pinto.Gen (CallResult(..), CastResult(..))
import Pinto.Gen as Gen
import Pinto.Timer as Timer

type StateServerName instanceData = ServerName (State instanceData) Msg

type StartArgs instanceData =
  { childStartLink :: StateServerName instanceData -> Effect StartLinkResult
  , childStopAction :: Maybe instanceData -> Effect Unit
  , serverName :: StateServerName instanceData
  , domain :: List Atom
  }

data Msg = InstanceDown Pid Foreign
         | ChildDown ExitMessage
         | Stop

type State instanceData =
  { instanceData :: Maybe instanceData
  , instancePid :: Maybe Pid
  , childStopAction :: Maybe instanceData -> Effect Unit
  , domain :: List Atom
  }

startLink :: forall instanceData. StartArgs instanceData -> Effect StartLinkResult
startLink startArgs@{serverName} = Gen.startLink serverName (init startArgs) handleInfo

getInstanceData :: forall instanceData. StateServerName instanceData -> Effect (Maybe instanceData)
getInstanceData serverName = do
  instancePid <- Erl.self
  Gen.doCall serverName (doGetInstanceData instancePid)
  where
    doGetInstanceData instancePid state@{instanceData} = do
      Gen.monitorPid serverName instancePid (\reason -> InstanceDown instancePid reason)
      pure $ CallReply instanceData state

recordInstanceData :: forall instanceData. StateServerName instanceData -> instanceData -> Effect Unit
recordInstanceData serverName instanceData = do
  Gen.call serverName doRecordInstanceData
  where
    doRecordInstanceData state =
      CallReply unit state{instanceData = Just instanceData}

init :: forall instanceData. StartArgs instanceData -> Effect (State instanceData)
init {serverName, childStartLink, childStopAction, domain} = do
  _ <- Erl.trapExit true
  logInfo domain "Persistent state starting child" {serverName}
  Gen.registerExternalMapping serverName ((map ChildDown) <<< Erl.exitMessageMapper)
  childResult <- childStartLink serverName
  case childResult of
    Ok pid -> do
      pure unit
    Ignore -> do
      logError domain "Child failed to start" { serverName
                                              , reason: Ignore}
      _ <- Timer.sendAfter serverName 0 Stop
      pure unit
    AlreadyStarted pid -> do
      logError domain "Child already started" { serverName
                                              , pid: pid}
      _ <- Timer.sendAfter serverName 0 Stop
      pure unit
    Failed reason -> do
      logError domain "Child failed to start" { serverName
                                              , reason: Failed reason}
      _ <- Timer.sendAfter serverName 0 Stop
      pure unit
  pure { instanceData: Nothing
       , instancePid: Nothing
       , childStopAction
       , domain
       }

handleInfo :: forall instanceData. Msg -> State instanceData -> Effect (CastResult (State instanceData))
handleInfo msg state@{instanceData, childStopAction, domain} = case msg of
  Stop ->
    -- This clause only exists because we can't return a stop from init
    pure $ CastStop state

  ChildDown (Exit pid reason) ->
    shutdown pid reason

  InstanceDown pid reason ->
    case Erl.mapExitReason reason of
      Normal ->
        shutdown pid reason

      Shutdown _ ->
        shutdown pid reason

      Other _ ->
        pure $ CastNoReply state
  where
    shutdown pid reason = do
      logInfo domain "State exiting due to child exit" { pid
                                                       , reason
                                                       , instanceData }
      childStopAction instanceData
      pure $ CastStop state

--------------------------------------------------------------------------------
-- Log helpers
--------------------------------------------------------------------------------
logInfo :: forall a. List Atom -> String -> a -> Effect Unit
logInfo domain msg misc = Logger.doLog domain Logger.info msg misc

logError :: forall a. List Atom -> String -> a -> Effect Unit
logError domain msg misc = Logger.doLog domain Logger.error msg misc
