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
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Erl.Atom (Atom, atom)
import Erl.Data.List (List)
import Erl.Data.Tuple (Tuple3, toNested3)
import Erl.Process (Process(..), SpawnedProcessState, spawnLink, (!))
import Erl.Process.Raw (Pid, receive)
import Erl.Utils (ExitMessage(..), ExitReason(..))
import Erl.Utils as Erl
import Foreign (Foreign, unsafeToForeign)
import Logger as Logger
import Pinto (ServerName, StartLinkResult(..))
import Pinto.Gen (CallResult(..), CastResult(..))
import Pinto.Gen as Gen

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
  , instancePid :: Maybe Pid
  , childStopAction :: Maybe instanceData -> Effect Unit
  , domain :: List Atom
  }

startLink :: forall instanceData. StartArgs instanceData -> Effect StartLinkResult
startLink startArgs@{serverName, domain} = do
  callerPid <- Erl.self
  let
    callerProcess = Process callerPid :: Process Atom
  logInfo domain "XXX Performing startup" {}
  startLinkResult <- Gen.startLink serverName (init callerProcess startArgs) handleInfo
  case startLinkResult of
    Ok _ -> do
      _ <- receive
      logInfo domain "XXX Startup completed" {}
      pure unit
    _ ->
      pure unit
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

init :: forall instanceData. (Process Atom) -> StartArgs instanceData -> Effect (State instanceData)
init caller {serverName, childStartLink, childStopAction, domain} = do
  _ <- Erl.trapExit true
  logInfo domain "Cached state starting child" {serverName}
  Gen.registerExternalMapping serverName ((map ChildDown) <<< Erl.exitMessageMapper)
  performInitialisation
  pure { instanceData: Nothing
       , instancePid: Nothing
       , childStopAction
       , domain
       }
  where
    performInitialisation = do
      logInfo domain "XXX about to spawnlink child helper" {}
      _ <- spawnLink launchChild
      pure unit
      where
        launchChild :: SpawnedProcessState (Tuple3 Atom Pid Foreign) -> Effect Unit
        launchChild {receive} = do
          _ <- Erl.trapExit true
          logInfo domain "XXX about to spawnlink child" {}
          childResult <- childStartLink serverName
          caller ! (atom "done")
          case childResult of
            Ok pid -> do
              Tuple _ (Tuple _ (Tuple reason _)) <- toNested3 <$> receive
              logInfo domain "XXX child exited" {reason}
              Erl.exit reason
              pure unit
            other -> do
              logError domain "Child failed to start" { serverName
                                                      , reason: other}
              Erl.exit $ unsafeToForeign $ atom "normal"
              pure $ unit

handleInfo :: forall instanceData. Msg instanceData -> State instanceData -> Effect (CastResult (State instanceData))
handleInfo msg state@{ instanceData
                     , childStopAction
                     , domain} = case msg of

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
