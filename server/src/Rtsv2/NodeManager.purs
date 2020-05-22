module Rtsv2.NodeManager
       ( startLink
       , getState
       , getAcceptingRequests
       , changeCanaryState
       , changeRunState
       , launchLocalAgent
       , launchLocalOrRemoteAgent
       , StartArgs
       ) where

import Prelude

import Data.Either (Either(..), either)
import Data.Foldable (foldl)
import Data.FoldableWithIndex (foldlWithIndex)
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Effect (Effect)
import Erl.Atom (Atom)
import Erl.Data.List (List, singleton)
import Erl.Data.Map (Map)
import Erl.Data.Map as Map
import Erl.Process.Raw (Pid)
import Erl.Utils as ErlUtils
import Logger (Logger)
import Logger as Logger
import Pinto (ServerName, StartLinkResult, ok')
import Pinto.Gen (CallResult(..), CastResult(..))
import Pinto.Gen as Gen
import Rtsv2.Agents.IntraPoP as IntraPoP
import Rtsv2.Config as Config
import Rtsv2.LoadTypes (ServerSelectionPredicate)
import Rtsv2.Names as Names
import Rtsv2.PoPDefinition as PoPDefinition
import Rtsv2.Utils (chainEither)
import Shared.Rtsv2.Agent (Agent(..))
import Shared.Rtsv2.Agent.State as PublicState
import Shared.Rtsv2.JsonLd as JsonLd
import Shared.Rtsv2.Types (class CanaryType, AcceptingRequests, AgentSupStartArgs, CanaryState(..), CanaryStateChangeFailure(..), LocalOrRemote(..), OnBehalfOf, ResourceFailed(..), ResourceResp, RunState(..), Server, canary)

data Msg =
  AgentDown Agent

type State =
  { args :: StartArgs
  , thisServer :: Server
  , currentRunState :: RunState
  , currentCanaryState :: CanaryState
  , agentCounts :: Map Agent Int
  , activeSupPid :: Pid
  }

data RequestType = CanaryRequest CanaryState
                 | OnBehalfOfRequest OnBehalfOf

--------------------------------------------------------------------------------
-- API
--------------------------------------------------------------------------------
type StartArgs =
  { activeSupStartLink :: AgentSupStartArgs -> Effect StartLinkResult
  }

startLink :: StartArgs -> Effect StartLinkResult
startLink args =
  Gen.startLink serverName (init args) handleInfo

getState :: Effect PublicState.NodeManager
getState =
  Gen.call serverName
  (\state@{ thisServer
          , currentRunState
          , currentCanaryState
          , agentCounts } ->
   let
     initialPublicState = { runState: currentRunState
                          , canaryState: currentCanaryState
                          , ingests: 0
                          , ingestAggregators: 0
                          , streamRelays: 0
                          , egests: 0
                          }

     publicState = foldlWithIndex (\agent acc count ->
                                    case agent of
                                      Ingest -> acc{ingests = count}
                                      IngestAggregator -> acc{ingestAggregators = count}
                                      StreamRelay -> acc{streamRelays = count}
                                      Egest -> acc{egests = count}
                                      _ -> acc
                                  )
                                  initialPublicState agentCounts
   in
    CallReply (JsonLd.nodeManagerStateNode publicState thisServer) state
  )

getAcceptingRequests :: Effect AcceptingRequests
getAcceptingRequests =
  Gen.call serverName (\state -> CallReply (acceptingRequests state.currentRunState) state)

getCanaryState :: Effect CanaryState
getCanaryState =
  Gen.call serverName (\state@{currentCanaryState} -> CallReply currentCanaryState state)

changeCanaryState :: CanaryState -> Effect (Either CanaryStateChangeFailure Unit)
changeCanaryState newCanary =
  Gen.doCall serverName doChange
  where
    doChange state@{currentCanaryState} | currentCanaryState == newCanary =
      pure $ CallReply (Left InvalidStateTransition) state

    doChange state@{agentCounts, activeSupPid, args: {activeSupStartLink}} =
      let
        totalAgents = foldl (\acc count -> acc + count) 0 agentCounts
      in
        if
          totalAgents > 0 then
            pure $ CallReply (Left ActiveAgents) state
        else do
          _ <- logInfo "CanaryState changed" { old: state.currentCanaryState
                                             , new: newCanary}
          ErlUtils.shutdown activeSupPid
          newActiveSupPid <- (ok' =<< (activeSupStartLink {canaryState: newCanary, acceptingRequestsFun: getAcceptingRequests}))
          pure $ CallReply (Right unit) state{ currentCanaryState = newCanary
                                             , activeSupPid = newActiveSupPid }

changeRunState :: RunState -> Effect (Either Unit Unit)
changeRunState newRunState =
  Gen.doCall serverName doChange
  where
    doChange state = do
      let
        state2 = state{ currentRunState = newRunState }
      _ <- logInfo "RunState changed" { old: state.currentRunState
                                      , new: newRunState}
      IntraPoP.announceAcceptingRequests $ acceptingRequests newRunState
      pure $ CallReply (Right unit) state2

launchLocalAgent :: forall canary. CanaryType canary => Agent -> canary -> ServerSelectionPredicate -> (Server -> Effect (Either ResourceFailed Pid)) -> Effect (ResourceResp Server)
launchLocalAgent agent canaryStream pred launchLocalFun = do
  idleServerResp <- IntraPoP.getThisIdleServer pred
  chainEither (launchAndUpdateState agent canaryStream launchLocalFun) idleServerResp

launchLocalOrRemoteAgent :: forall canary. CanaryType canary => Agent -> canary -> ServerSelectionPredicate -> (Server -> Effect (Either ResourceFailed Pid)) -> (Server -> Effect Boolean) -> Effect (ResourceResp Server)
launchLocalOrRemoteAgent agent canaryStream pred launchLocalFun launchRemoteFun = do
  canary <- getCanaryState
  case canary of
    Canary -> launchLocalAgent agent canaryStream pred launchLocalFun
    Live -> do
      idleServerResp <- IntraPoP.getIdleServer pred
      chainEither launch idleServerResp
      where
        launch (Local thisServer) = launchAndUpdateState agent canaryStream launchLocalFun thisServer
        launch (Remote remote) = do
          resp <- launchRemoteFun remote
          pure $ if resp then Right (Remote remote)
                 else Left LaunchFailed

--------------------------------------------------------------------------------
-- Gen-server callbacks
--------------------------------------------------------------------------------
init :: StartArgs -> Effect State
init args@{activeSupStartLink} = do
  { initialRunState
  , initialCanaryState } <- Config.nodeManagerConfig
  thisServer <- PoPDefinition.getThisServer
  activeSupPid <- (ok' =<< (activeSupStartLink {canaryState: initialCanaryState, acceptingRequestsFun: getAcceptingRequests}))

  let
    state = { args
            , thisServer
            , currentRunState: initialRunState
            , currentCanaryState: initialCanaryState
            , agentCounts: Map.empty
            , activeSupPid
            }
  IntraPoP.announceAcceptingRequests $ acceptingRequests initialRunState

  pure state

handleInfo :: Msg -> State -> Effect (CastResult State)
handleInfo msg state@{agentCounts, currentRunState} =
  case msg of
    AgentDown agent -> do
      let
        newAgentCounts = Map.alter decrementAgentCount agent agentCounts
      newRunState <- if (Map.size newAgentCounts) == 0 && (currentRunState == PassiveDrain || currentRunState == ForceDrain) then do
                      _ <- logInfo "RunState changed" { old: state.currentRunState
                                                      , new: OutOfService}
                      IntraPoP.announceAcceptingRequests $ acceptingRequests OutOfService
                      pure OutOfService
                    else
                      pure currentRunState
      pure $ CastNoReply state{ agentCounts = newAgentCounts
                              , currentRunState = newRunState}
  where
    decrementAgentCount Nothing = Nothing
    decrementAgentCount (Just 1) = Nothing
    decrementAgentCount (Just n) = Just (n - 1)

--------------------------------------------------------------------------------
-- Internal Functions
--------------------------------------------------------------------------------
serverName :: ServerName State Msg
serverName = Names.nodeManagerServerName

acceptingRequests :: RunState -> AcceptingRequests
acceptingRequests currentRunState =
  wrap $ currentRunState == Active

launchAndUpdateState :: forall canary. CanaryType canary => Agent -> canary -> (Server -> Effect (Either ResourceFailed Pid)) -> Server -> Effect (ResourceResp Server)
launchAndUpdateState agent canaryStream launchFun thisServer =
  Gen.doCall serverName doLaunchAndUpdateState
  where
    doLaunchAndUpdateState state@{currentRunState} | currentRunState /= Active =
      pure $ CallReply (Left InvalidRunState) state

    doLaunchAndUpdateState state@{currentCanaryState} | currentCanaryState /= canary currentCanaryState canaryStream =
      pure $ CallReply (Left InvalidCanaryState) state

    doLaunchAndUpdateState state@{agentCounts} = do
      launchResp <- launchFun thisServer
      either (failure state) (success state) launchResp

    success state@{agentCounts} pid = do
      Gen.monitorPid serverName pid (\_ -> AgentDown agent)
      pure $ CallReply (Right (Local thisServer)) state{agentCounts = Map.alter incrementAgentCount agent agentCounts}
    failure state reason =
      pure $ CallReply (Left reason) state
    incrementAgentCount Nothing = Just 1
    incrementAgentCount (Just n) = Just (n + 1)

domains :: List Atom
domains = serverName # Names.toDomain # singleton

logInfo :: forall a. Logger (Record a)
logInfo = Logger.doLog domains Logger.info
