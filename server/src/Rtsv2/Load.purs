module Rtsv2.Load
       ( startLink
       , load
       , setLoad
       ) where

import Prelude
import Rtsv2.LoadTypes

import Data.Foldable (foldl)
import Data.Int (round, toNumber)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap, wrap)
import Data.Set (fromFoldable)
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Erl.Atom (Atom)
import Erl.Data.List (List, singleton)
import Logger (Logger)
import Logger as Logger
import Pinto (ServerName, StartLinkResult)
import Pinto.Gen (CallResult(..), CastResult(..))
import Pinto.Gen as Gen
import Pinto.Timer as Timer
import Rtsv2.Agents.IntraPoP as IntraPop
import Rtsv2.Config (LoadConfig)
import Rtsv2.Config as Config
import Rtsv2.Names as Names
import Shared.Rtsv2.Agent (AggregatorSerfPayload)
import Shared.Rtsv2.Types (Load, NetworkBPS(..), Percentage(..), Server(..), SpecInt(..))

type CurrentLoad =
  { currentCPU :: Percentage
  , currentNetwork :: NetworkBPS
  }

hasCapacityForEgestInstance :: AggregatorSerfPayload -> Server -> CurrentLoad -> LoadConfig -> LoadCheckResult
hasCapacityForEgestInstance serfPayload targetServer currentLoad {costs: LoadCosts {egest: agentCosts}, limits} =
  hasCapacity serfPayload targetServer currentLoad agentCosts limits

hasCapacityForEgestClient :: AggregatorSerfPayload -> Server -> CurrentLoad -> LoadConfig -> LoadCheckResult
hasCapacityForEgestClient serfPayload targetServer currentLoad {costs: LoadCosts {egest: agentCosts}, limits} =
  hasCapacity serfPayload targetServer currentLoad agentCosts limits

hasCapacityForStreamRelay :: AggregatorSerfPayload -> Server -> CurrentLoad -> LoadConfig -> LoadCheckResult
hasCapacityForStreamRelay serfPayload targetServer currentLoad {costs: LoadCosts {streamRelay: agentCosts}, limits} =
  hasCapacity serfPayload targetServer currentLoad agentCosts limits

hasCapacityForAggregator :: AggregatorSerfPayload -> Server -> CurrentLoad -> LoadConfig -> LoadCheckResult
hasCapacityForAggregator serfPayload targetServer currentLoad {costs: LoadCosts {ingestAggregator: agentCosts}, limits} =
  hasCapacity serfPayload targetServer currentLoad agentCosts limits

hasCapacityForRtmpIngest :: AggregatorSerfPayload -> Server -> CurrentLoad -> LoadConfig -> LoadCheckResult
hasCapacityForRtmpIngest serfPayload targetServer currentLoad {costs: LoadCosts {webRTCIngest: agentCosts}, limits} =
  hasCapacity serfPayload targetServer currentLoad agentCosts limits

hasCapacityForWebRTCIngest :: AggregatorSerfPayload -> Server -> CurrentLoad -> LoadConfig -> LoadCheckResult
hasCapacityForWebRTCIngest serfPayload targetServer currentLoad {costs: LoadCosts {rtmpIngest: agentCosts}, limits} =
  hasCapacity serfPayload targetServer currentLoad agentCosts limits

type State =
  {
    load :: Load
  }

data Msg = Tick

serverName :: ServerName State Msg
serverName = Names.loadServerName

startLink :: Unit -> Effect StartLinkResult
startLink args =
  Gen.startLink serverName (init args) handleInfo

load :: Effect Load
load =
  Gen.call serverName \state@{ load: currentLoad } -> CallReply currentLoad state

setLoad :: Number -> Effect Unit
setLoad newLoad = do
  IntraPop.announceLoad (wrap newLoad)
  Gen.call serverName \state -> CallReply unit state{load = (wrap newLoad)}

init :: Unit -> Effect State
init args = do
  logInfo "Load monitor starting" {}
  config <- Config.loadConfig
  void $ Timer.sendEvery serverName config.loadAnnounceMs Tick
  pure $ {load: (wrap 0.0)}

handleInfo :: Msg -> State -> Effect (CastResult State)
handleInfo msg state@{load: currentLoad} =
  case msg of
    Tick ->
      do
        IntraPop.announceLoad currentLoad
        pure $ CastNoReply state

--------------------------------------------------------------------------------
-- Internals
--------------------------------------------------------------------------------
hasCapacity :: AggregatorSerfPayload -> Server -> CurrentLoad -> LoadAgentCosts -> LoadLimits -> LoadCheckResult
hasCapacity serfPayload@(Tuple numStreams bitrate) targetServer currentLoad agentCosts limits =
  let
    {currentCPU, currentNetwork} = currentLoad
    Server {maxCpuCapacity, maxNetworkCapacity} = targetServer
    LoadLimits {cpu: cpuLimits, network: networkLimits} = limits

    LoadFixedCost {cpu: additionalCpu, network: additionalNetwork} = agentCostsToFixed serfPayload targetServer agentCosts
    proposedCpu = currentCPU <> (cpuUnitsToPercentage additionalCpu maxCpuCapacity)
    proposedNetwork = networkToPercentage (currentNetwork <> additionalNetwork) maxNetworkCapacity
    cpuResult = checkWatermark proposedCpu cpuLimits
    networkResult = checkWatermark proposedNetwork networkLimits
  in
   max cpuResult networkResult

agentCostsToFixed :: AggregatorSerfPayload -> Server -> LoadAgentCosts -> LoadFixedCost
agentCostsToFixed serfPayload@(Tuple numStreams bitrate) targetServer agentCosts =
  let
    LoadAgentCosts {fixed, perStream, perBPS, hardwareFactors} = agentCosts
    perStreamFixed = perStreamToFixed perStream numStreams
    perBitrateFixed = perBitrateToFixed perBPS bitrate
    standardFixed = fixed <> perStreamFixed <> perBitrateFixed
  in
    adjustForHardwareCapabilites standardFixed targetServer hardwareFactors

perStreamToFixed :: LoadVariableCost -> Int -> LoadFixedCost
perStreamToFixed variable numStreams =
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

adjustForHardwareCapabilites :: LoadFixedCost -> Server -> List HardwareFactor -> LoadFixedCost
adjustForHardwareCapabilites initialCost (Server {capabilityTags}) hardwareFactors =
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

networkToPercentage :: NetworkBPS -> NetworkBPS -> Percentage
networkToPercentage (NetworkBPS x) (NetworkBPS y) = Percentage ((toNumber x) * 100.0 / (toNumber y))


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
