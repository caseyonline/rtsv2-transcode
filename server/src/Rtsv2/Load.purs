module Rtsv2.Load
       ( startLink
       , load
       , setLoad
       ) where

import Prelude
import Rtsv2.LoadTypes

import Data.Int (round, toNumber)
import Data.Newtype (wrap)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Erl.Atom (Atom)
import Erl.Data.List (List, singleton)
import Erl.Data.Tuple (uncurry2)
import Logger (Logger)
import Logger as Logger
import Pinto (ServerName, StartLinkResult)
import Pinto.Gen (CallResult(..), CastResult(..))
import Pinto.Gen as Gen
import Pinto.Timer as Timer
import Rtsv2.Agents.IntraPoP as IntraPop
import Rtsv2.Config (LoadConfig)
import Rtsv2.Config as Config
import Rtsv2.Env as Env
import Rtsv2.Names as Names
import Shared.Agent (AggregatorSerfPayload)
import Shared.Types (Load, NetworkBPS(..), Percentage(..), SpecInt(..))

-- decaying things
-- monitor actual load - cpu %age and network bps


type CurrentLoad =
  { currentCPU :: Percentage
  , currentNetwork :: NetworkBPS
  }

hasCapacityForEgestInstance :: AggregatorSerfPayload -> CurrentLoad -> LoadConfig -> Boolean
hasCapacityForEgestInstance serfPayload currentLoad loadConfig = false

hasCapacityForEgestClient :: AggregatorSerfPayload -> CurrentLoad -> LoadConfig -> Boolean
hasCapacityForEgestClient serfPayload currentLoad loadConfig = false

hasCapacityForStreamRelay :: AggregatorSerfPayload -> CurrentLoad -> LoadConfig -> Boolean
hasCapacityForStreamRelay serfPayload currentLoad loadConfig = false

hasCapacityForAggregator :: AggregatorSerfPayload -> CurrentLoad -> LoadConfig -> Boolean
hasCapacityForAggregator serfPayload currentLoad loadConfig = false

hasCapacityForRtmpIngest :: AggregatorSerfPayload -> CurrentLoad -> LoadConfig -> Boolean
hasCapacityForRtmpIngest serfPayload currentLoad loadConfig = false

hasCapacityForWebRTCIngest :: AggregatorSerfPayload -> CurrentLoad -> LoadConfig -> LoadCheckResult
hasCapacityForWebRTCIngest serfPayload currentLoad config@{costs: LoadCosts {webRTCIngest}} =
  hasCapacity (agentCostsToFixed webRTCIngest serfPayload) currentLoad config

hasCapacity :: LoadFixedCosts -> CurrentLoad -> LoadConfig -> LoadCheckResult
hasCapacity (LoadFixedCosts { cpu: additionalCpu
                            , network: additionalNetwork }) { currentCPU
                                                            , currentNetwork } { limits: LoadLimits {cpu: cpuLimits, network: networkLimits}} =
  let
    -- TODO needs to get passed a server so that it know capacity, hardware tags etc
    maxCpuCapacity = wrap 1.0
    maxNetworkCapacity = wrap 1
    proposedCpu = currentCPU <> (cpuUnitsToPercentage additionalCpu maxCpuCapacity)
    proposedNetwork = networkToPercentage (currentNetwork <> additionalNetwork) maxNetworkCapacity
    cpuResult = checkWatermark proposedCpu cpuLimits
    networkResult = checkWatermark proposedNetwork networkLimits
  in
   max cpuResult networkResult

checkWatermark :: Percentage -> LoadWatermarks -> LoadCheckResult
checkWatermark percentage (LoadWatermarks {lowWaterMark, highWaterMark}) =
  if percentage < lowWaterMark then Green
  else if percentage < highWaterMark then Amber
       else Red

agentCostsToFixed :: LoadAgentCosts -> AggregatorSerfPayload -> LoadFixedCosts
agentCostsToFixed (LoadAgentCosts {fixed, variable}) streamCosts =
  fixed <> (variableCostsToFixed variable streamCosts)

variableCostsToFixed :: LoadVariableCosts -> AggregatorSerfPayload -> LoadFixedCosts
variableCostsToFixed (LoadVariableCosts {cpu, network}) streamCosts =
  LoadFixedCosts { cpu: SpecInt $ variableCostToFixed cpu streamCosts
                 , network: NetworkBPS $ round $ variableCostToFixed network streamCosts
                 }

variableCostToFixed :: LoadVariableCost -> AggregatorSerfPayload -> Number
variableCostToFixed (LoadVariableCost variable) (Tuple numStreams totalBitrate) =
  uncurry2 (\numStreamFactor bitrateFactor -> (numStreamFactor * (toNumber numStreams)) + (bitrateFactor * (toNumber totalBitrate))) variable

cpuUnitsToPercentage :: SpecInt -> SpecInt -> Percentage
cpuUnitsToPercentage (SpecInt x) (SpecInt y) = Percentage (x * 100.0 / y)

networkToPercentage :: NetworkBPS -> NetworkBPS -> Percentage
networkToPercentage (NetworkBPS x) (NetworkBPS y) = Percentage ((toNumber x) * 100.0 / (toNumber y))

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
