module Rtsv2.NodeManager
       ( startLink
       , getState
       , runIfValidState
       , launchIfValidState
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
import Erl.Data.Map (Map)
import Erl.Data.Map as Map
import Erl.Process.Raw (Pid)
import Erl.Utils as ErlUtils
import Foreign (Foreign)
import Logger (spy)
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
import Shared.Rtsv2.Types (AcceptingRequests, Canary(..), CanaryStateChangeFailure(..), LocalOrRemote(..), ResourceFailed(..), ResourceResp, RunState(..), Server)

data Msg =
  AgentDown Agent

type State =
  { args :: StartArgs
  , thisServer :: Server
  , currentRunState :: RunState
  , currentCanaryState :: Canary
  , agentCounts :: Map Agent Int
  , activeSupPid :: Pid
  }

--------------------------------------------------------------------------------
-- API
--------------------------------------------------------------------------------
type StartArgs =
  { activeSupStartLink :: Canary -> Effect StartLinkResult
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

getCanaryState :: Effect Canary
getCanaryState =
  Gen.call serverName (\state@{currentCanaryState} -> CallReply currentCanaryState state)

launchIfValidState :: forall a. Canary -> Effect (ResourceResp a) -> Effect (ResourceResp a)
launchIfValidState canary launch =
  foo <$> runIfValidState canary launch
  where
    foo (Left err) = Left err
    foo (Right err@(Left _)) = err
    foo (Right ok@(Right _)) = ok

runIfValidState :: forall a. Canary -> Effect a -> Effect (Either ResourceFailed a)
runIfValidState canary launch = do
  currentCanary <- getCanaryState
  if canary == currentCanary then
    Right <$> launch
  else
    pure $ Left InvalidCanaryState

changeCanaryState :: Canary -> Effect (Either CanaryStateChangeFailure Unit)
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
          ErlUtils.shutdown activeSupPid
          newActiveSupPid <- (ok' =<< (activeSupStartLink newCanary))
          pure $ CallReply (Right unit) state{ currentCanaryState = newCanary
                                             , activeSupPid = newActiveSupPid }

changeRunState :: RunState -> Effect (Either Unit Unit)
changeRunState newRunState =
  Gen.doCall serverName doChange
  where
    doChange state = do
      let
        state2 = state{ currentRunState = newRunState }
      IntraPoP.announceAcceptingRequests $ acceptingRequests state
      pure $ CallReply (Right unit) state2

launchLocalAgent :: Agent -> ServerSelectionPredicate -> (Server -> Effect (Either ResourceFailed Pid)) -> Effect (ResourceResp Server)
launchLocalAgent agent pred launchLocalFun = do
  idleServerResp <- IntraPoP.getThisIdleServer pred
  chainEither (launchAndUpdateState agent launchLocalFun) idleServerResp

launchLocalOrRemoteAgent :: Agent -> ServerSelectionPredicate -> (Server -> Effect (Either ResourceFailed Pid)) -> (Server -> Effect Boolean) -> Effect (ResourceResp Server)
launchLocalOrRemoteAgent agent pred launchLocalFun launchRemoteFun = do
  canary <- getCanaryState
  case canary of
    Canary -> launchLocalAgent agent pred launchLocalFun
    Live -> do
      idleServerResp <- IntraPoP.getIdleServer pred
      chainEither launch idleServerResp
      where
        launch (Local thisServer) = launchAndUpdateState agent launchLocalFun thisServer
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
  activeSupPid <- (ok' =<< (activeSupStartLink initialCanaryState))

  let
    state = { args
            , thisServer
            , currentRunState: initialRunState
            , currentCanaryState: initialCanaryState
            , agentCounts: Map.empty
            , activeSupPid
            }
  IntraPoP.announceAcceptingRequests $ acceptingRequests state

  pure state

handleInfo :: Msg -> State -> Effect (CastResult State)
handleInfo msg state@{agentCounts} =
  case msg of
    AgentDown agent ->
      pure $ CastNoReply state{agentCounts = Map.alter decrementAgentCount agent agentCounts}
  where
    decrementAgentCount Nothing = Nothing
    decrementAgentCount (Just 1) = Nothing
    decrementAgentCount (Just n) = Just (n - 1)

--------------------------------------------------------------------------------
-- Internal Functions
--------------------------------------------------------------------------------
serverName :: ServerName State Msg
serverName = Names.nodeManagerServerName

acceptingRequests :: State -> AcceptingRequests
acceptingRequests { currentRunState } =
  wrap $ currentRunState == Active

launchAndUpdateState :: Agent -> (Server -> Effect (Either ResourceFailed Pid)) -> Server -> Effect (ResourceResp Server)
launchAndUpdateState agent launchFun thisServer =
  Gen.doCall serverName
  (\state@{agentCounts} ->  do
      launchResp <- launchFun thisServer
      either (failure state) (success state) launchResp
  )
  where
    success state@{agentCounts} pid = do
      Gen.monitorPid serverName pid (\_ -> AgentDown agent)
      pure $ CallReply (Right (Local thisServer)) state{agentCounts = Map.alter incrementAgentCount agent agentCounts}
    failure state reason =
      pure $ CallReply (Left reason) state
    incrementAgentCount Nothing = Just 1
    incrementAgentCount (Just n) = Just (n + 1)
