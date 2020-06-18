module Rtsv2.Agents.CachedInstanceState
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
import Erl.Process (Process(..))
import Erl.Process.Raw (Pid)
import Erl.Utils (ExitMessage(..), ExitReason(..), Ref)
import Erl.Utils as Erl
import Foreign (Foreign)
import Logger as Logger
import Pinto (ServerName, StartLinkResult(..))
import Pinto.Gen (CallResult(..), CastResult(..), TerminateReason)
import Pinto.Gen as Gen

foreign import receiveImpl :: Ref -> Effect Unit
foreign import spawnChild :: Pid -> Process Ref -> Ref -> Effect StartLinkResult -> Effect Pid

type StateServerName instanceData = ServerName (State instanceData) (Msg instanceData)

type StartArgs instanceData =
  { childStartLink :: StateServerName instanceData -> Effect StartLinkResult
  , childStopAction :: Maybe instanceData -> Effect Unit
  , serverName :: StateServerName instanceData
  , domain :: List Atom
  }

data Msg instanceData =
    InstanceDown Pid Foreign
  | ChildDown ExitMessage

type State instanceData =
  { instanceData :: Maybe instanceData
  , instancePid :: Pid
  , childStopAction :: Maybe instanceData -> Effect Unit
  , domain :: List Atom
  }

startLink :: forall instanceData. StartArgs instanceData -> Effect StartLinkResult
startLink startArgs@{serverName, domain} = do
  callerPid <- Erl.self
  ref <- Erl.makeRef
  let
    callerProcess = Process callerPid :: Process Ref
  startLinkResult <- Gen.startLink serverName (init callerProcess ref startArgs) handleInfo
  case startLinkResult of
    Ok _ -> do
      receiveImpl ref
      pure startLinkResult
    _ ->
      pure startLinkResult

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

init :: forall instanceData. (Process Ref) -> Ref -> StartArgs instanceData -> Effect (State instanceData)
init caller ref {serverName, childStartLink, childStopAction, domain} = do
  void $ Erl.trapExit true
  logInfo domain "Cached state starting child" {serverName}
  Gen.registerExternalMapping serverName ((map ChildDown) <<< Erl.exitMessageMapper)
  Gen.registerTerminate serverName terminate
  self <- Erl.self
  instancePid <- spawnChild self caller ref $ childStartLink serverName
  pure { instanceData: Nothing
       , instancePid
       , childStopAction
       , domain
       }

handleInfo :: forall instanceData. Msg instanceData -> State instanceData -> Effect (CastResult (State instanceData))
handleInfo msg state@{ instanceData
                     , childStopAction
                     , domain} =
  case msg of
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

terminate :: forall instanceData. TerminateReason -> State instanceData -> Effect Unit
terminate reason state@{ domain, instanceData, childStopAction, instancePid } =
  case reason of
    Gen.Normal ->
      pure unit
    _ ->
      do
        logWarning domain "Cached instance state terminating" {reason, instancePid}
        Erl.shutdown instancePid
        childStopAction instanceData
        pure unit

--------------------------------------------------------------------------------
-- Log helpers
--------------------------------------------------------------------------------
logInfo :: forall report. List Atom -> String -> Record report -> Effect Unit
logInfo domain = Logger.info <<< Logger.traceMetadata domain

logWarning :: forall report. List Atom -> String -> Record report -> Effect Unit
logWarning domain = Logger.warning <<< Logger.traceMetadata domain

logError :: forall report. List Atom -> String -> Record report -> Effect Unit
logError domain = Logger.error <<< Logger.traceMetadata domain
