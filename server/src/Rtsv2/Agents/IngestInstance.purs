module Rtsv2.Agents.IngestInstance
   ( startLink
  , isActive
  , setClientMetadata
  , setSourceInfo
  , getPublicState
  , stopIngest
  , StartArgs
  ) where

import Prelude

import Bus as Bus
import Data.Either (Either(..), hush)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap, wrap)
import Data.Traversable (sequence)
import Effect (Effect)
import Erl.Atom (Atom, atom)
import Erl.Data.List (List, nil, (:))
import Erl.Process.Raw (Pid)
import Erl.Utils (systemTimeMs)
import Logger (Logger)
import Logger as Logger
import Pinto (ServerName, StartLinkResult)
import Pinto as Pinto
import Pinto.Gen (CallResult(..), CastResult(..))
import Pinto.Gen as Gen
import Pinto.Timer as Timer
import Rtsv2.Agents.IngestAggregatorInstance as IngestAggregatorInstance
import Rtsv2.Agents.IngestAggregatorInstanceSup as IngestAggregatorInstanceSup
import Rtsv2.Agents.IngestStats as IngestStats
import Rtsv2.Agents.IntraPoP (IntraPoPBusMessage(..), launchLocalOrRemoteGeneric)
import Rtsv2.Agents.IntraPoP as IntraPoP
import Rtsv2.Agents.Locator (extractServer)
import Rtsv2.Agents.Locator.Types (LocalOrRemote(..), ResourceResp)
import Rtsv2.Audit as Audit
import Rtsv2.Config as Config
import Rtsv2.Names as Names
import Rtsv2.PoPDefinition as PoPDefinition
import Rtsv2.Router.Endpoint (Endpoint(..), makeUrl)
import Rtsv2.Utils (crashIfLeft)
import Shared.Agent as Agent
import Shared.LlnwApiTypes (StreamDetails, StreamPublish)
import Shared.Stream (AggregatorKey, IngestKey(..), ingestKeyToAggregatorKey)
import Shared.Types (Load, Milliseconds, Server, ServerLoad(..), extractAddress)
import Shared.Types.Agent.State as PublicState
import Shared.Types.Media.Types.Rtmp (RtmpClientMetadata)
import Shared.Types.Media.Types.SourceDetails (SourceInfo)
import SpudGun (Url)
import SpudGun as SpudGun

serverName :: IngestKey -> ServerName State Msg
serverName ingestKey = Names.ingestInstanceName ingestKey

data Msg
   = WriteEqLog
   | InformAggregator
   | IntraPoPBus IntraPoP.IntraPoPBusMessage
   | HandlerDown

type State
  = { thisServer :: Server
    , aggregatorRetryTime :: Milliseconds
    , ingestKey :: IngestKey
    , streamPublish :: StreamPublish
    , streamDetails :: StreamDetails
    , ingestStartedTime :: Milliseconds
    , lastIngestAuditTime :: Milliseconds
    , remoteAddress :: String
    , remotePort :: Int
    , localPort :: Int
    , aggregatorAddr :: Maybe (LocalOrRemote Server)
    , clientMetadata :: Maybe (RtmpClientMetadata List)
    , sourceInfo :: Maybe (SourceInfo List)
    }

type StartArgs
  = { streamPublish :: StreamPublish
    , streamDetails :: StreamDetails
    , ingestKey :: IngestKey
    , remoteAddress :: String
    , remotePort :: Int
    , handlerPid :: Pid
    }

startLink :: StartArgs -> Effect StartLinkResult
startLink args@{ingestKey} = Gen.startLink (serverName ingestKey) (init args) handleInfo

isActive :: IngestKey -> Effect Boolean
isActive ingestKey = Pinto.isRegistered (serverName ingestKey)

setClientMetadata :: IngestKey -> (RtmpClientMetadata List) -> Effect Unit
setClientMetadata ingestKey metadata =
  Gen.doCall (serverName ingestKey) \state -> do
    pure $ CallReply unit state{clientMetadata = Just metadata}

setSourceInfo :: IngestKey -> (SourceInfo List) -> Effect Unit
setSourceInfo ingestKey sourceInfo =
  Gen.doCall (serverName ingestKey) \state -> do
    pure $ CallReply unit state{sourceInfo = Just sourceInfo}

stopIngest :: IngestKey -> Effect Unit
stopIngest ingestKey =
  Gen.doCall (serverName ingestKey) \state -> do
    doStopIngest state
    pure $ CallStop unit state

getPublicState :: IngestKey -> Effect (PublicState.Ingest List)
getPublicState ingestKey =
  Gen.call (serverName ingestKey) \state@{ clientMetadata
                                         , sourceInfo
                                         , remoteAddress
                                         , remotePort
                                         , ingestStartedTime} -> do
    CallReply { rtmpClientMetadata: clientMetadata
              , sourceInfo
              , remoteAddress
              , remotePort
              , ingestStartedTime } state

init :: StartArgs -> Effect State
init { streamPublish
     , streamDetails
     , ingestKey
     , remoteAddress
     , remotePort
     , handlerPid} = do

  logInfo "Ingest starting" {ingestKey, handlerPid}

  thisServer <- PoPDefinition.getThisServer
  now <- systemTimeMs
  {port: localPort} <- Config.rtmpIngestConfig
  {intraPoPLatencyMs} <- Config.globalConfig
  {eqLogIntervalMs} <- Config.ingestInstanceConfig

  Gen.monitorPid ourServerName handlerPid (\_ -> HandlerDown)
  void $ Bus.subscribe ourServerName IntraPoP.bus IntraPoPBus
  void $ Timer.sendAfter ourServerName 0 InformAggregator
  void $ Timer.sendEvery ourServerName eqLogIntervalMs WriteEqLog

  pure { thisServer
       , streamPublish
       , streamDetails
       , ingestKey
       , aggregatorRetryTime: wrap intraPoPLatencyMs
       , aggregatorAddr: Nothing
       , clientMetadata: Nothing
       , sourceInfo: Nothing
       , remoteAddress
       , remotePort
       , localPort
       , ingestStartedTime: now
       , lastIngestAuditTime: now
       }
  where
    ourServerName = (serverName ingestKey)

handleInfo :: Msg -> State -> Effect (CastResult State)
handleInfo msg state@{ingestKey} = case msg of
  WriteEqLog -> do
    eqLine <- ingestEqLine state
    Audit.ingestUpdate eqLine
    pure $ CastNoReply state{lastIngestAuditTime = eqLine.endMs}

  InformAggregator -> do
    state2 <- informAggregator state
    pure $ CastNoReply state2

  IntraPoPBus (IngestAggregatorExited aggregatorKey serverAddress) -> do
    logInfo "exit" {aggregatorKey, serverAddress, ingestKey: state.ingestKey}
    state2 <- handleAggregatorExit aggregatorKey serverAddress state
    pure $ CastNoReply state2

  HandlerDown -> do
    logInfo "RTMP Handler has exited" {ingestKey}
    doStopIngest state
    pure $ CastStop state

ingestEqLine :: State -> Effect Audit.IngestEqLine
ingestEqLine state@{ ingestKey
                   , streamPublish: { host: ingestIp
                                    , protocol: connectionType
                                    , shortname
                                    , streamName
                                    , username }
                   , streamDetails: { role }
                   , localPort: ingestPort
                   , remoteAddress: userIp
                   , lastIngestAuditTime: startMs} = do
  endMs <- systemTimeMs
  stats <- IngestStats.getStatsForIngest ingestKey
  let
    metrics = _.rtmpIngestMetrics <$> stats
    totalBytesSent = fromMaybe 0 ((_.totalBytesSent) <$> metrics)
    totalBytesReceived = fromMaybe 0 ((_.totalBytesReceived) <$> metrics)
  pure { ingestIp
       , ingestPort
       , userIp
       , username
       , shortname
       , streamName
       , streamRole: role
       , connectionType
       , startMs
       , endMs
       , bytesWritten: totalBytesSent
       , bytesRead: totalBytesReceived
       , lostPackets: 0
       }

doStopIngest :: State -> Effect Unit
doStopIngest state@{aggregatorAddr, ingestKey} = do
  removeVariant ingestKey aggregatorAddr
  eqLine <- ingestEqLine state
  Audit.ingestStop eqLine
  pure unit

informAggregator :: State -> Effect State
informAggregator state@{streamDetails, ingestKey, thisServer, aggregatorRetryTime} = do
  maybeAggregator <- hush <$> getAggregator streamDetails ingestKey
  maybeVariantAdded <- sequence ((addVariant thisServer ingestKey) <$> (extractServer <$>  maybeAggregator))
  case fromMaybe false maybeVariantAdded of
    true -> pure state{aggregatorAddr = maybeAggregator}
    false -> do
      void $ Timer.sendAfter (serverName ingestKey) (unwrap aggregatorRetryTime) InformAggregator
      pure state

handleAggregatorExit :: AggregatorKey -> Server -> State -> Effect State
handleAggregatorExit exitedAggregatorKey exitedAggregatorAddr state@{ingestKey, aggregatorRetryTime, aggregatorAddr}
  | exitedAggregatorKey == (ingestKeyToAggregatorKey ingestKey) && Just exitedAggregatorAddr == (extractServer <$> aggregatorAddr) = do
      void $ Timer.sendAfter (serverName ingestKey) 0 InformAggregator
      pure state
  | otherwise =
      pure state

addVariant :: Server -> IngestKey -> Server -> Effect Boolean
addVariant thisServer ingestKey aggregatorAddress
  | aggregatorAddress == thisServer = do
    IngestAggregatorInstance.addVariant ingestKey
    pure true
  | otherwise = do
    let
      -- TODO - functions to make URLs from Server
      url = makeActiveIngestUrl aggregatorAddress ingestKey
    restResult <- SpudGun.postJson url $ extractAddress thisServer
    case restResult of
      Left _ -> pure $ false
      Right _ -> pure $ true

makeActiveIngestUrl :: Server -> IngestKey -> Url
makeActiveIngestUrl server (IngestKey streamId streamRole streamVariant) =
  makeUrl server $ IngestAggregatorActiveIngestsE streamId streamRole streamVariant

removeVariant :: IngestKey -> Maybe (LocalOrRemote Server)-> Effect Unit
removeVariant ingestKey Nothing = pure unit
removeVariant ingestKey (Just (Local aggregator)) = do
    IngestAggregatorInstance.removeVariant ingestKey
    pure unit
removeVariant ingestKey (Just (Remote aggregator)) = do
  let
    url = makeActiveIngestUrl aggregator ingestKey
  void $ crashIfLeft =<< SpudGun.delete url {}

getAggregator :: StreamDetails -> IngestKey -> Effect (ResourceResp Server)
getAggregator streamDetails ingestKey = do
  maybeAggregator <- IntraPoP.whereIsIngestAggregator (ingestKeyToAggregatorKey ingestKey)
  case maybeAggregator of
    Just server ->
      pure $ Right $ Local server
    Nothing ->
      launchLocalOrRemote streamDetails ingestKey

launchLocalOrRemote :: StreamDetails -> IngestKey -> Effect (ResourceResp Server)
launchLocalOrRemote streamDetails ingestKey = do
  launchLocalOrRemoteGeneric filterForAggregatorLoad launchLocal launchRemote
  where
    launchLocal :: ServerLoad -> Effect Unit
    launchLocal _ = do
      void $ IngestAggregatorInstanceSup.startAggregator streamDetails
      pure unit
    launchRemote idleServer = do
      let
        url = makeUrl idleServer IngestAggregatorsE
      void $ crashIfLeft =<< SpudGun.postJson url streamDetails


loadThresholdToCreateAggregator :: Load
loadThresholdToCreateAggregator = wrap 50.0

filterForAggregatorLoad :: ServerLoad -> Boolean
filterForAggregatorLoad (ServerLoad sl) = sl.load < loadThresholdToCreateAggregator

--------------------------------------------------------------------------------
-- Log helpers
--------------------------------------------------------------------------------
domains :: List Atom
domains = atom <$> (show Agent.Ingest :  "Instance" : nil)

logInfo :: forall a. Logger a
logInfo = domainLog Logger.info

--logWarning :: forall a. Logger a
--logWarning = domainLog Logger.warning

domainLog :: forall a. Logger {domain :: List Atom, misc :: a} -> Logger a
domainLog = Logger.doLog domains
