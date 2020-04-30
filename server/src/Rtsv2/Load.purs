module Rtsv2.Load
       ( startLink
       , load
       , setLoad
       , addPredictedLoad

       , hasCapacityForEgestInstance
       , hasCapacityForEgestClient
       , hasCapacityForStreamRelay
       , hasCapacityForAggregator
       , hasCapacityForRtmpIngest
       , hasCapacityForWebRTCIngest
       , launchLocalGeneric
       , launchLocalOrRemoteGeneric
       ) where

import Prelude

import Data.Array (elemIndex)
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.FoldableWithIndex (foldlWithIndex) as Map
import Data.Int (round, toNumber)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap, wrap)
import Data.Set (fromFoldable)
import Data.Set as Set
import Effect (Effect)
import Erl.Atom (Atom)
import Erl.Data.List (List, singleton)
import Erl.Data.Map as Map
import Erl.Data.Tuple (Tuple2, uncurry2)
import Erl.Utils as Erl
import Logger (Logger)
import Logger as Logger
import Pinto (ServerName, StartLinkResult)
import Pinto.Gen (CallResult(..), CastResult(..))
import Pinto.Gen as Gen
import Pinto.Timer as Timer
import Rtsv2.Agents.IntraPoP as IntraPoP
import Rtsv2.Agents.IntraPoP as IntraPop
import Rtsv2.Config (LoadConfig)
import Rtsv2.Config as Config
import Rtsv2.LoadTypes (HardwareFactor(..), LoadAgentCosts(..), LoadCheckResult(..), LoadCosts(..), LoadFixedCost(..), LoadLimits(..), PredictedLoad(..), LoadVariableCost(..), LoadWatermarks(..), ServerSelectionPredicate)
import Rtsv2.Names as Names
import Rtsv2.PoPDefinition as PoPDefinition
import Shared.Common (Milliseconds)
import Shared.Rtsv2.Agent (Agent(..), SlotCharacteristics)
import Shared.Rtsv2.Stream (AgentKey)
import Shared.Rtsv2.Types (CurrentLoad(..), LocalOrRemote(..), NetworkKbps(..), Percentage(..), ResourceFailed(..), ResourceResp, Server(..), ServerLoad(..), SpecInt(..), minLoad)

launchLocalGeneric :: ServerSelectionPredicate -> (Server -> Effect Boolean) -> Effect (ResourceResp Server)
launchLocalGeneric pred launchLocal = do
  idleServerResp <- IntraPoP.getThisIdleServer pred
  launchResp <- launch idleServerResp
  pure $ launchResp
  where
    launch (Left err) = pure (Left err)
    launch (Right server) = do
      resp <- launchLocal server
      pure $ if resp then Right (Local server)
             else Left LaunchFailed

launchLocalOrRemoteGeneric :: ServerSelectionPredicate -> (Server -> Effect Boolean) -> (Server -> Effect Boolean) -> Effect (ResourceResp Server)
launchLocalOrRemoteGeneric pred launchLocal launchRemote = do
  idleServerResp <- IntraPoP.getIdleServer pred
  launchResp <- launch idleServerResp
  pure $ launchResp
  where
    launch :: Either ResourceFailed (LocalOrRemote Server) -> Effect (ResourceResp Server)
    launch (Left err) = pure (Left err)
    launch (Right resource) = do
      resp <- launch' resource
      pure $ if resp then Right resource
             else Left LaunchFailed
    launch' (Local local) = launchLocal local
    launch' (Remote remote) = launchRemote remote

hasCapacityForEgestInstance :: SlotCharacteristics -> LoadConfig -> ServerLoad -> LoadCheckResult
hasCapacityForEgestInstance =
  hasCapacity _.egestInstance Egest

hasCapacityForEgestClient :: SlotCharacteristics -> LoadConfig -> ServerLoad -> LoadCheckResult
hasCapacityForEgestClient =
  hasCapacity _.egestClient Egest

hasCapacityForStreamRelay :: SlotCharacteristics -> LoadConfig -> ServerLoad -> LoadCheckResult
hasCapacityForStreamRelay  =
  hasCapacity _.streamRelay StreamRelay

hasCapacityForAggregator :: SlotCharacteristics -> LoadConfig -> ServerLoad -> LoadCheckResult
hasCapacityForAggregator =
  hasCapacity _.ingestAggregator IngestAggregator

hasCapacityForRtmpIngest :: SlotCharacteristics -> LoadConfig -> ServerLoad -> LoadCheckResult
hasCapacityForRtmpIngest =
  hasCapacity _.rtmpIngest Ingest

hasCapacityForWebRTCIngest :: SlotCharacteristics -> LoadConfig -> ServerLoad -> LoadCheckResult
hasCapacityForWebRTCIngest =
  hasCapacity _.webRTCIngest Ingest

foreign import data CpuState :: Type
foreign import cpuUtilInitImpl :: Effect CpuState
foreign import cpuUtilImpl :: CpuState -> Effect (Tuple2 Number CpuState)

foreign import data NetState :: Type
foreign import networkUtilInitImpl :: Effect NetState
foreign import networkUtilImpl :: NetState -> Effect (Tuple2 Int NetState)

type DecayingLoad = { currentAdditionalCpu :: Percentage
                    , currentAdditionalNetwork :: NetworkKbps
                    , cpuDecayPerTick :: Percentage
                    , networkDecayPerTick :: NetworkKbps
                    }

type State =
  { config :: LoadConfig
  , thisServer :: Server
  , load :: CurrentLoad
  , cpuUtilState :: CpuState
  , networkUtilState :: NetState
  , predictedLoads :: Map.Map AgentKey DecayingLoad
  , tickTime :: Milliseconds
  }

data Msg = Tick

serverName :: ServerName State Msg
serverName = Names.loadServerName

startLink :: Unit -> Effect StartLinkResult
startLink args =
  Gen.startLink serverName (init args) handleInfo

load :: Effect CurrentLoad
load =
  Gen.call serverName \state@{ load: currentLoad } -> CallReply currentLoad state

setLoad :: CurrentLoad -> Effect Unit
setLoad newLoad = do
  Gen.doCall serverName doSetLoad
  where
    doSetLoad state@{config: {monitorLoad}} | monitorLoad == false = do
      IntraPop.announceLoad newLoad
      pure $ CallReply unit state{load = newLoad}
    doSetLoad state | otherwise =
      pure $ CallReply unit state

addPredictedLoad :: AgentKey -> PredictedLoad -> Effect Unit
addPredictedLoad key (PredictedLoad {cost: (LoadFixedCost {cpu, network}), decayTime}) =
  Gen.call serverName doAddPredictedLoad
  where
    doAddPredictedLoad state@{predictedLoads, tickTime, thisServer: Server {maxCpuCapacity}} =
      let
        currentAdditionalCpu = cpuUnitsToPercentage cpu maxCpuCapacity
        newLoad = { currentAdditionalCpu
                  , currentAdditionalNetwork: network
                  , cpuDecayPerTick: wrap $ (unwrap currentAdditionalCpu) * (unwrap tickTime) / (unwrap decayTime)
                  , networkDecayPerTick: wrap $ (unwrap network) * ((round <<< unwrap) tickTime) / ((round <<< unwrap) decayTime)
                  }
      in
        CallReply unit state{predictedLoads = Map.insert key newLoad predictedLoads}

init :: Unit -> Effect State
init args = do
  logInfo "Load monitor starting" {}
  thisServer <- PoPDefinition.getThisServer
  config@{loadAnnounceMs} <- Config.loadConfig
  cpuUtilState <- cpuUtilInitImpl
  networkUtilState <- networkUtilInitImpl
  void $ Timer.sendEvery serverName loadAnnounceMs Tick
  pure $ { config
         , thisServer
         , load: minLoad
         , cpuUtilState
         , networkUtilState
         , predictedLoads: Map.empty
         , tickTime: wrap (toNumber loadAnnounceMs)
         }

handleInfo :: Msg -> State -> Effect (CastResult State)
handleInfo msg state@{ load: currentLoad@(CurrentLoad {currentNetwork})
                     , config: {monitorLoad}} =
  case msg of
    Tick | monitorLoad ->
      do
        now <- Erl.vmTimeMs
        {cpu: newCpu, state: state2} <- getCurrentCpu state
        {network: newNetwork, state: state3} <- getCurrentNetwork state2
        let
          newMeasuredLoad = CurrentLoad { currentCpu: newCpu
                                        , currentNetwork: newNetwork
                                        }
          state4 = decayPredicted state3
          newLoad = addPredicted newMeasuredLoad state4
        IntraPop.announceLoad newLoad
        pure $ CastNoReply state4{load = newLoad}
    Tick | otherwise -> do
      IntraPop.announceLoad currentLoad
      pure $ CastNoReply state

getCurrentCpu :: State -> Effect { cpu :: Percentage
                                 , state :: State }
getCurrentCpu state@{cpuUtilState} = do
  res <- cpuUtilImpl cpuUtilState
  uncurry2 (\util cpuUtilState2 -> do
               pure {cpu: wrap util, state: state{cpuUtilState = cpuUtilState2}}) res

getCurrentNetwork :: State -> Effect { network :: NetworkKbps
                                     , state :: State }
getCurrentNetwork state@{networkUtilState} = do
  res <- networkUtilImpl networkUtilState
  uncurry2 (\mkbps networkUtilState2 -> do
               pure {network: wrap mkbps, state: state{networkUtilState = networkUtilState2}}) res

--------------------------------------------------------------------------------
-- Internals
--------------------------------------------------------------------------------
maybeStartTimer :: LoadConfig -> Effect Unit
maybeStartTimer {monitorLoad, loadAnnounceMs} | monitorLoad == true =
  void $ Timer.sendEvery serverName loadAnnounceMs Tick

maybeStartTimer {monitorLoad, loadAnnounceMs} | otherwise =
  pure unit

hasCapacity :: ({ egestClient :: LoadAgentCosts
                , egestInstance :: LoadAgentCosts
                , ingestAggregator :: LoadAgentCosts
                , rtmpIngest :: LoadAgentCosts
                , streamRelay :: LoadAgentCosts
                , webRTCIngest :: LoadAgentCosts
                } -> LoadAgentCosts) -> Agent -> SlotCharacteristics -> LoadConfig -> ServerLoad -> LoadCheckResult
hasCapacity extractor agent slotCharacteristics {limits: defaultLimits, costs: LoadCosts costs} targetServer@(ServerLoad {agents}) =
  case elemIndex agent agents of
    Just _ ->
      let
        agentCosts@(LoadAgentCosts {limitOverrides}) = extractor costs
        limits = fromMaybe defaultLimits limitOverrides
        ServerLoad {maxCpuCapacity, maxNetworkCapacity, load} = targetServer
        CurrentLoad {currentCpu, currentNetwork} = load
        LoadLimits {cpu: cpuLimits, network: networkLimits} = limits

        LoadFixedCost {cpu: additionalCpu, network: additionalNetwork} = agentCostsToFixed slotCharacteristics targetServer agentCosts
        proposedCpu = currentCpu <> (cpuUnitsToPercentage additionalCpu maxCpuCapacity)
        proposedNetwork = networkToPercentage (currentNetwork <> additionalNetwork) maxNetworkCapacity
        cpuResult = checkWatermark proposedCpu cpuLimits
        networkResult = checkWatermark proposedNetwork networkLimits
      in
       max cpuResult networkResult
    Nothing ->
      Red

agentCostsToFixed :: SlotCharacteristics -> ServerLoad -> LoadAgentCosts -> LoadFixedCost
agentCostsToFixed slotCharacteristics@{numProfiles, totalBitrate} targetServer agentCosts =
  let
    LoadAgentCosts {fixed, perProfile, perKbps, hardwareFactors} = agentCosts
    perProfileFixed = perProfileToFixed perProfile numProfiles
    perBitrateFixed = perBitrateToFixed perKbps totalBitrate
    standardFixed = fixed <> perProfileFixed <> perBitrateFixed
  in
    adjustForHardwareCapabilites standardFixed targetServer hardwareFactors

perProfileToFixed :: LoadVariableCost -> Int -> LoadFixedCost
perProfileToFixed variable numStreams =
  let
    LoadVariableCost {cpu, network} = variable
  in
    LoadFixedCost { cpu: wrap $ (unwrap cpu) * (toNumber numStreams)
                  , network: wrap $ (unwrap network) * numStreams
                  }

perBitrateToFixed :: LoadVariableCost -> Int -> LoadFixedCost
perBitrateToFixed variable bitrate =
  let
    LoadVariableCost {cpu, network} = variable
  in
    LoadFixedCost { cpu: wrap $ (unwrap cpu) * (toNumber bitrate)
                  , network: wrap $ (unwrap network) * bitrate
                  }

adjustForHardwareCapabilites :: LoadFixedCost -> ServerLoad -> List HardwareFactor -> LoadFixedCost
adjustForHardwareCapabilites initialCost (ServerLoad {capabilityTags}) hardwareFactors =
  foldl applyHardwareCapability initialCost hardwareFactors
  where
    applyHardwareCapability cost (HardwareFactor {name, cpuFactor, networkFactor}) =
      case Set.member name serverSet of
        false -> cost
        true ->
          applyNetworkFactor networkFactor $ applyCpuFactor cpuFactor cost

    serverSet = fromFoldable capabilityTags

    applyNetworkFactor Nothing cost = cost
    applyNetworkFactor (Just factor) (LoadFixedCost cost@{network}) = LoadFixedCost cost{network = wrap $ round $ ((toNumber <<< unwrap) network) * factor}

    applyCpuFactor Nothing cost = cost
    applyCpuFactor (Just factor) (LoadFixedCost cost@{cpu}) = LoadFixedCost cost{cpu = wrap $ (unwrap cpu) * factor}

checkWatermark :: Percentage -> LoadWatermarks -> LoadCheckResult
checkWatermark percentage (LoadWatermarks {lowWaterMark, highWaterMark}) =
  if percentage < lowWaterMark then Green
  else if percentage < highWaterMark then Amber
       else Red

cpuUnitsToPercentage :: SpecInt -> SpecInt -> Percentage
cpuUnitsToPercentage (SpecInt x) (SpecInt y) = Percentage (x * 100.0 / y)

networkToPercentage :: NetworkKbps -> NetworkKbps -> Percentage
networkToPercentage (NetworkKbps x) (NetworkKbps y) = Percentage ((toNumber x) * 100.0 / (toNumber y))

decayPredicted :: State -> State
decayPredicted state@{predictedLoads} =
  state{predictedLoads = Map.foldlWithIndex decayPotential Map.empty predictedLoads}
  where
    decayPotential key acc predictedLoad@{ currentAdditionalCpu
                                         , currentAdditionalNetwork
                                         , cpuDecayPerTick
                                         , networkDecayPerTick} =
      let
        newCpu = wrap $ max 0.0 $ (unwrap currentAdditionalCpu) - (unwrap cpuDecayPerTick)
        newNetwork = wrap $ max 0 $ (unwrap currentAdditionalNetwork) - (unwrap networkDecayPerTick)
      in
        if
          newCpu == wrap 0.0 && newNetwork == wrap 0 then acc
        else Map.insert key predictedLoad{ currentAdditionalCpu = newCpu
                                         , currentAdditionalNetwork = newNetwork} acc

addPredicted :: CurrentLoad -> State -> CurrentLoad
addPredicted currentLoad state@{predictedLoads} =
  foldl addPredicted' currentLoad predictedLoads
  where
    addPredicted' (CurrentLoad {currentCpu, currentNetwork}) {currentAdditionalCpu, currentAdditionalNetwork} =
      CurrentLoad { currentCpu: currentCpu <> currentAdditionalCpu
                  , currentNetwork: currentNetwork <> currentAdditionalNetwork
                  }

--------------------------------------------------------------------------------
-- Log helpers
--------------------------------------------------------------------------------
domains :: List Atom
domains = serverName # Names.toDomain # singleton

logInfo :: forall a. Logger (Record a)
logInfo = domainLog Logger.info

logWarning :: forall a. Logger (Record a)
logWarning = domainLog Logger.warning

domainLog :: forall a. Logger {domain :: List Atom, misc :: Record a} -> Logger (Record a)
domainLog = Logger.doLog domains
