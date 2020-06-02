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
import Erl.Process.Raw (Pid)
import Erl.Utils (ExitMessage(..), ExitReason(..), Ref)
import Erl.Utils as Erl
import Foreign (Foreign, unsafeToForeign)
import Logger as Logger
import Pinto (ServerName, StartLinkResult(..))
import Pinto.Gen (CallResult(..), CastResult(..), TerminateReason)
import Pinto.Gen as Gen
import Prim.Row as Row

foreign import receiveImpl :: Ref -> Effect Unit

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
  ref <- Erl.makeRef
  let
    callerProcess = Process callerPid :: Process Ref
  startLinkResult <- Gen.startLink serverName (init callerProcess ref startArgs) handleInfo
  Gen.registerTerminate serverName terminate
  case startLinkResult of
    Ok _ -> do
      _ <- receiveImpl ref
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
      _ <- spawnLink launchChild
      pure unit
      where
        launchChild :: SpawnedProcessState (Tuple3 Atom Pid Foreign) -> Effect Unit
        launchChild {receive} = do
          _ <- Erl.trapExit true
          childResult <- childStartLink serverName
          caller ! ref
          case childResult of
            Ok pid -> do
              Tuple _ (Tuple _ (Tuple reason _)) <- toNested3 <$> receive
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

terminate :: forall instanceData. TerminateReason -> State instanceData -> Effect Unit
terminate reason state@{ domain, instanceData, childStopAction } =
  case reason of
    Gen.Normal ->
      pure unit
    _ ->
      do
        logWarning domain "Cached instance state terminating" {reason}
        childStopAction instanceData
        pure unit

--------------------------------------------------------------------------------
-- Log helpers
--------------------------------------------------------------------------------
logInfo :: forall a. Row.Lacks "text" a => List Atom -> String -> Record a -> Effect Unit
logInfo domain msg misc = Logger.doLog domain Logger.info msg misc

logWarning :: forall a. Row.Lacks "text" a => List Atom -> String -> Record a -> Effect Unit
logWarning domain msg misc = Logger.doLog domain Logger.warning msg misc

logError :: forall a. Row.Lacks "text" a => List Atom -> String -> Record a -> Effect Unit
logError domain msg misc = Logger.doLog domain Logger.error msg misc
