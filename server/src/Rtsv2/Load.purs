module Rtsv2.Load
       ( startLink
       , load
       , setLoad
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
import Data.Int (round, toNumber)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap, wrap)
import Data.Set (fromFoldable)
import Data.Set as Set
import Effect (Effect)
import Erl.Atom (Atom)
import Erl.Data.List (List, singleton)
import Erl.Data.Tuple (Tuple2, uncurry2)
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
import Rtsv2.LoadTypes (HardwareFactor(..), LoadAgentCosts(..), LoadCheckResult(..), LoadCosts(..), LoadFixedCost(..), LoadLimits(..), LoadVariableCost(..), LoadWatermarks(..), ServerSelectionPredicate)
import Rtsv2.Names as Names
import Shared.Rtsv2.Agent (Agent(..), SlotCharacteristics)
import Shared.Rtsv2.Types (CurrentLoad(..), LocalOrRemote(..), NetworkKbps(..), Percentage(..), ResourceFailed(..), ResourceResp, Server, ServerLoad(..), SpecInt(..), minLoad)

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
foreign import networkUtilImpl :: NetState -> Effect (Tuple2 (Maybe Int) NetState)

type State =
  { config :: LoadConfig
  , load :: CurrentLoad
  , cpuUtilState :: CpuState
  , networkUtilState :: NetState
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

init :: Unit -> Effect State
init args = do
  logInfo "Load monitor starting" {}
  config@{loadAnnounceMs} <- Config.loadConfig
  cpuUtilState <- cpuUtilInitImpl
  networkUtilState <- networkUtilInitImpl
  void $ Timer.sendEvery serverName loadAnnounceMs Tick
  pure $ { config
         , load: minLoad
         , cpuUtilState
         , networkUtilState
         }

handleInfo :: Msg -> State -> Effect (CastResult State)
handleInfo msg state@{ load: currentLoad@(CurrentLoad {currentNetwork})
                     , config: {monitorLoad}} =
  case msg of
    Tick | monitorLoad ->
      do
        {cpu: newCpu, state: state2} <- getCurrentCpu state
        {network: newNetwork, state: state3} <- getCurrentNetwork state2
        let
          newLoad = CurrentLoad { currentCpu: newCpu
                                , currentNetwork: fromMaybe currentNetwork newNetwork
                                }
        IntraPop.announceLoad newLoad
        pure $ CastNoReply state3{load = newLoad}
    Tick | otherwise -> do
      IntraPop.announceLoad currentLoad
      pure $ CastNoReply state

getCurrentCpu :: State -> Effect { cpu :: Percentage
                                 , state :: State }
getCurrentCpu state@{cpuUtilState} = do
  res <- cpuUtilImpl cpuUtilState
  uncurry2 (\util cpuUtilState2 -> do
               pure {cpu: wrap util, state: state{cpuUtilState = cpuUtilState2}}) res

getCurrentNetwork :: State -> Effect { network :: Maybe NetworkKbps
                                     , state :: State }
getCurrentNetwork state@{networkUtilState} = do
  res <- networkUtilImpl networkUtilState
  uncurry2 (\mkbps networkUtilState2 -> do
               pure {network: wrap <$> mkbps, state: state{networkUtilState = networkUtilState2}}) res

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
