module Rtsv2.NodeManager
       ( startLink
       , getState
       , getAcceptingRequests
       , changeCanaryState
       , changeRunState
       , launchLocalAgent
       , launchLocalOrRemoteAgent
       , getIngestKeys
       , getIngestAggregatorKeys
       , getStreamRelayKeys
       , getEgestKeys
       , StartArgs
       ) where

import Prelude

import Data.Either (Either(..), either)
import Data.Foldable (foldl)
import Data.FoldableWithIndex (foldlWithIndex)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (wrap)
import Data.Traversable (traverse)
import Effect (Effect)
import Erl.Atom (Atom)
import Erl.Data.List (List, singleton)
import Erl.Data.Map (Map)
import Erl.Data.Map as Map
import Erl.Process.Raw (Pid)
import Erl.Utils (Ref)
import Erl.Utils as Erl
import Erl.Utils as ErlUtils
import Logger (spy)
import Logger as Logger
import Pinto (ServerName, StartLinkResult, ok')
import Pinto.Gen (CallResult(..), CastResult(..))
import Pinto.Gen as Gen
import Pinto.Timer as Timer
import Prim.Row as Row
import Rtsv2.Agents.EgestInstance as EgestInstance
import Rtsv2.Agents.IngestAggregatorInstance as IngestAggregatorInstance
import Rtsv2.Agents.IntraPoP as IntraPoP
import Rtsv2.Agents.StreamRelayInstance as StreamRelayInstance
import Rtsv2.Config as Config
import Rtsv2.LoadTypes (ServerSelectionPredicate)
import Rtsv2.Names as Names
import Rtsv2.PoPDefinition as PoPDefinition
import Rtsv2.Types (LocalOrRemote(..), LocalResource(..), LocalResourceResp, ResourceFailed(..), ResourceResp, AgentSupStartArgs)
import Rtsv2.Utils (chainEither)
import Shared.Rtsv2.Agent (Agent(..))
import Shared.Rtsv2.Agent.State as PublicState
import Shared.Rtsv2.JsonLd as JsonLd
import Shared.Rtsv2.Stream (AggregatorKey, EgestKey, IngestKey, RelayKey)
import Shared.Rtsv2.Types (class CanaryType, AcceptingRequests, CanaryState(..), CanaryStateChangeFailure(..), OnBehalfOf, RunState(..), RunStateChangeFailure(..), Server, canary)

data Msg =
  AgentDown Agent
  | ForceDrainTimeout Ref

type State =
  { args :: StartArgs
  , config :: Config.NodeManagerConfig
  , thisServer :: Server
  , currentRunState :: RunState
  , currentCanaryState :: CanaryState
  , agentCounts :: Map Agent Int
  , activeSupPid :: Pid
  , forceDrainPhase :: ForceDrainPhase
  , forceDrainTimeoutRef :: Maybe Ref
  }

data RequestType = CanaryRequest CanaryState
                 | OnBehalfOfRequest OnBehalfOf

data ForceDrainPhase = DrainAggregators
                     | WaitAggregators
                     | DrainRelays
                     | WaitRelays
                     | DrainEgests
                     | WaitEgests

foreign import getAgentKeys :: forall a. String -> Effect (List a)

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
      pure $ CallReply (Left InvalidCanaryStateTransition) state

    doChange state@{currentRunState} | currentRunState == ForceDrain =
      pure $ CallReply (Left InvalidCanaryStateTransition) state

    doChange state@{agentCounts, activeSupPid, args: {activeSupStartLink}} =
      let
        totalAgents = foldl (\acc count -> acc + count) 0 agentCounts
      in
        if
          totalAgents > 0 then
            pure $ CallReply (Left ActiveAgents) state
        else do
          logCommand "CanaryState changed" { old: state.currentCanaryState
                                           , new: newCanary}
          let
            state2 = state{ currentCanaryState = newCanary }
          (CallReply (Right unit)) <$> restartActiveSup state2

changeRunState :: RunState -> Effect (Either RunStateChangeFailure Unit)
changeRunState newRunState =
  Gen.doCall serverName doChange
  where
    doChange state@{currentRunState} | currentRunState == ForceDrain, newRunState == Active = pure $ CallReply (Left InvalidRunStateTransition) state
    doChange state@{currentRunState} | currentRunState == ForceDrain, newRunState == PassiveDrain = pure $ CallReply (Left InvalidRunStateTransition) state
    doChange state@{currentRunState} | currentRunState == OutOfService, newRunState == PassiveDrain = pure $ CallReply (Left InvalidRunStateTransition) state
    doChange state@{currentRunState} | currentRunState == OutOfService, newRunState == ForceDrain = pure $ CallReply (Left InvalidRunStateTransition) state
    doChange state = do
      let
        state2 = state{ currentRunState = newRunState }
      _ <- logCommand "RunState changed" { old: state.currentRunState
                                         , new: newRunState}
      IntraPoP.announceAcceptingRequests $ acceptingRequests newRunState
      state3 <- maybeForceDrain state2
      pure $ CallReply (Right unit) state3

launchLocalAgent :: forall canary. CanaryType canary => Agent -> canary -> ServerSelectionPredicate -> (Server -> Effect (Either ResourceFailed Pid)) -> Effect (LocalResourceResp Server)
launchLocalAgent agent canaryStream pred launchLocalFun = do
  idleServerResp <- IntraPoP.getThisIdleServer pred
  chainEither (launchAndUpdateState agent canaryStream launchLocalFun) idleServerResp

launchLocalOrRemoteAgent :: forall canary. CanaryType canary => Agent -> canary -> ServerSelectionPredicate -> (Server -> Effect (Either ResourceFailed Pid)) -> (Server -> Effect Boolean) -> Effect (ResourceResp Server)
launchLocalOrRemoteAgent agent canaryStream pred launchLocalFun launchRemoteFun = do
  canary <- getCanaryState
  case canary of
    Canary -> (map toLocalAndRemote) <$> launchLocalAgent agent canaryStream pred launchLocalFun
    Live -> do
      idleServerResp <- IntraPoP.getIdleServer pred
      chainEither launch (spy "launch" idleServerResp)
      where
        launch :: LocalOrRemote Server -> Effect (ResourceResp Server)
        launch (Local thisServer) = (map toLocalAndRemote) <$> launchAndUpdateState agent canaryStream launchLocalFun thisServer
        launch (Remote remote) = do
          resp <- launchRemoteFun remote
          pure $ if resp then Right (Remote remote)
                 else Left LaunchFailed
  where
    toLocalAndRemote (LocalResource _pid a) = Local a

getIngestKeys :: Effect (List IngestKey)
getIngestKeys = getAgentKeys (show Ingest)

getIngestAggregatorKeys :: Effect (List AggregatorKey)
getIngestAggregatorKeys = getAgentKeys (show IngestAggregator)

getStreamRelayKeys :: Effect (List RelayKey)
getStreamRelayKeys = getAgentKeys (show StreamRelay)

getEgestKeys :: Effect (List EgestKey)
getEgestKeys = getAgentKeys (show Egest)

--------------------------------------------------------------------------------
-- Gen-server callbacks
--------------------------------------------------------------------------------
init :: StartArgs -> Effect State
init args@{activeSupStartLink} = do
  config@{ initialRunState
         , initialCanaryState } <- Config.nodeManagerConfig
  thisServer <- PoPDefinition.getThisServer
  activeSupPid <- (ok' =<< (activeSupStartLink (activeSupStartArgs initialCanaryState)))

  let
    state = { args
            , config
            , thisServer
            , currentRunState: initialRunState
            , currentCanaryState: initialCanaryState
            , agentCounts: Map.empty
            , activeSupPid
            , forceDrainPhase: DrainAggregators
            , forceDrainTimeoutRef: Nothing
            }
  IntraPoP.announceAcceptingRequests $ acceptingRequests initialRunState

  pure state

handleInfo :: Msg -> State -> Effect (CastResult State)
handleInfo msg state@{agentCounts, currentRunState, forceDrainTimeoutRef} =
  case msg of
    AgentDown agent -> do
      let
        newAgentCounts = Map.alter decrementAgentCount agent agentCounts
        state2 = state{ agentCounts = newAgentCounts }

      CastNoReply <$>
        if currentRunState == ForceDrain then
           maybeForceDrain state2
        else
          if (Map.size newAgentCounts) == 0 && (currentRunState == PassiveDrain) then
            transitionToOutOfService state2
          else
            pure state2

    ForceDrainTimeout ref
      | Just ref == forceDrainTimeoutRef -> do
        logInfo "Force drain timeout" {}
        CastNoReply <$> transitionToOutOfService state
      | otherwise ->
        CastNoReply <$> pure state

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

launchAndUpdateState :: forall canary. CanaryType canary => Agent -> canary -> (Server -> Effect (Either ResourceFailed Pid)) -> Server -> Effect (LocalResourceResp Server)
launchAndUpdateState agent canaryStream launchFun thisServer =
  Gen.doCall serverName doLaunchAndUpdateState
  where
    doLaunchAndUpdateState state@{currentRunState} | currentRunState /= Active =
      pure $ CallReply (Left InvalidRunState) state

    doLaunchAndUpdateState state@{currentCanaryState} | currentCanaryState /= canary currentCanaryState canaryStream = do
      pure $ CallReply (Left InvalidCanaryState) state

    doLaunchAndUpdateState state@{agentCounts} = do
      launchResp <- launchFun thisServer
      either (failure state) (success state) launchResp

    success state@{agentCounts} pid = do
      Gen.monitorPid serverName pid (\_ -> AgentDown agent)
      pure $ CallReply (Right (LocalResource pid thisServer)) state{agentCounts = Map.alter incrementAgentCount agent agentCounts}
    failure state reason =
      pure $ CallReply (Left reason) state
    incrementAgentCount Nothing = Just 1
    incrementAgentCount (Just n) = Just (n + 1)

maybeForceDrain :: State -> Effect State
maybeForceDrain state@{ currentRunState: ForceDrain
                      , forceDrainPhase: DrainAggregators
                      , agentCounts
                      , config: { forceDrainTimeoutMs }
                      } = do
  ref <- Erl.makeRef
  void $ Timer.sendAfter serverName forceDrainTimeoutMs (ForceDrainTimeout ref)
  logInfo "Initiating force drain" {agentCounts}

  if numAggregators state == 0 then
    maybeForceDrain state{ forceDrainPhase = DrainRelays
                         , forceDrainTimeoutRef = Just ref
                         }
  else do
    logInfo "Performing aggregator force drain" {}
    -- Step 1 - call forceDrain on aggregators - they will each:
       -- launch a new aggregator
       -- announce that they have stopped (but without stopping)
       -- stop when they see the announcement that the new aggregator has started
    aggregators <- getIngestAggregatorKeys
    _ <- traverse IngestAggregatorInstance.forceDrain aggregators
    pure state{ forceDrainPhase = WaitAggregators
              , forceDrainTimeoutRef = Just ref
              }

maybeForceDrain state@{ currentRunState: ForceDrain
                      , forceDrainPhase: WaitAggregators
                      , agentCounts
                      } =
  if numAggregators state == 0 then
    maybeForceDrain state{forceDrainPhase = DrainRelays}
  else do
    pure state

maybeForceDrain state@{ currentRunState: ForceDrain
                      , forceDrainPhase: DrainRelays
                      , agentCounts
                      } =
  if numRelays state == 0 then
    maybeForceDrain state{forceDrainPhase = DrainEgests}
  else do
    logInfo "Performing relay force drain" {}
    -- Step 1 - call forceDrain on relays - they will each:
    relays <- getStreamRelayKeys
    _ <- traverse StreamRelayInstance.forceDrain relays
    pure state{forceDrainPhase = WaitRelays}

maybeForceDrain state@{ currentRunState: ForceDrain
                      , forceDrainPhase: WaitRelays
                      , agentCounts
                      } =
  if numRelays state == 0 then
    maybeForceDrain state{forceDrainPhase = DrainEgests}
  else do
    pure state

maybeForceDrain state@{ currentRunState: ForceDrain
                      , forceDrainPhase: DrainEgests
                      , agentCounts
                      } =
  if numEgests state == 0 then
    transitionToOutOfService state
  else do
    logInfo "Performing egest force drain" {}
    -- Step 1 - call forceDrain on egests - they will each:
    egests <- getEgestKeys
    _ <- traverse EgestInstance.forceDrain egests
    pure state{forceDrainPhase = WaitEgests}

maybeForceDrain state@{ currentRunState: ForceDrain
                      , forceDrainPhase: WaitEgests
                      , agentCounts
                      } =
  if numEgests state == 0 then
    transitionToOutOfService state
  else do
    pure state

maybeForceDrain state =
  pure state

transitionToOutOfService :: State -> Effect State
transitionToOutOfService state = do
  _ <- logInfo "RunState is OutOfService" { old: state.currentCanaryState }
  restartActiveSup state{ currentRunState = OutOfService
                        , forceDrainPhase = DrainAggregators }

restartActiveSup :: State -> Effect State
restartActiveSup state@{activeSupPid, args: {activeSupStartLink}, currentCanaryState} = do
  ErlUtils.shutdown activeSupPid
  newActiveSupPid <- (ok' =<< (activeSupStartLink (activeSupStartArgs currentCanaryState)))
  pure state{ activeSupPid = newActiveSupPid }

numAggregators :: State -> Int
numAggregators { agentCounts} =
  (fromMaybe 0) (Map.lookup IngestAggregator agentCounts)

numRelays :: State -> Int
numRelays { agentCounts} =
  (fromMaybe 0) (Map.lookup StreamRelay agentCounts)

numEgests :: State -> Int
numEgests { agentCounts} =
  (fromMaybe 0) (Map.lookup Egest agentCounts)

activeSupStartArgs :: CanaryState -> AgentSupStartArgs
activeSupStartArgs canaryState =
  { canaryState
  , acceptingRequestsFun: getAcceptingRequests
  }

domains :: List Atom
domains = serverName # Names.toDomain # singleton

logInfo :: forall report. Row.Lacks "text" report => String -> { | report } -> Effect Unit
logInfo = Logger.doLog domains Logger.info

logCommand :: forall report. Row.Lacks "text" report => String -> { | report } -> Effect Unit
logCommand = Logger.doLogCommand domains
