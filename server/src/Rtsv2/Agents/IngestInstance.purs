module Rtsv2.Agents.IngestInstance
   ( startLink
   , stopAction
   , isActive
   , setClientMetadata
   , setSourceInfo
   , getPublicState
   , stopIngest
   , StartArgs

   , PersistentState
   , StateServerName
   , domain
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
import Rtsv2.Agents.IngestAggregatorSup as IngestAggregatorSup
import Rtsv2.Agents.IngestStats as IngestStats
import Rtsv2.Agents.IntraPoP (IntraPoPBusMessage(..), launchLocalOrRemoteGeneric)
import Rtsv2.Agents.IntraPoP as IntraPoP
import Rtsv2.Agents.Locator (extractServer)
import Rtsv2.Agents.Locator.Types (LocalOrRemote(..), ResourceResp)
import Rtsv2.Agents.PersistentInstanceState as PersistentInstanceState
import Rtsv2.Audit as Audit
import Rtsv2.Config as Config
import Rtsv2.Names as Names
import Rtsv2.PoPDefinition as PoPDefinition
import Rtsv2.Utils (crashIfLeft)
import Shared.Agent as Agent
import Shared.Common (Milliseconds, Url)
import Shared.LlnwApiTypes (StreamDetails, StreamPublish(..))
import Shared.Router.Endpoint (Endpoint(..), makeUrl)
import Shared.Stream (AggregatorKey, IngestKey(..), ingestKeyToAggregatorKey)
import Shared.Types (Load, Server, ServerLoad(..), extractAddress)
import Shared.Types.Agent.State as PublicState
import Shared.Types.Media.Types.Rtmp (RtmpClientMetadata)
import Shared.Types.Media.Types.SourceDetails (SourceInfo)
import SpudGun as SpudGun

serverName :: IngestKey -> ServerName State Msg
serverName ingestKey = Names.ingestInstanceName ingestKey

data Msg
   = WriteEqLog
   | InformAggregator
   | IntraPoPBus IntraPoP.IntraPoPBusMessage
   | HandlerDown

type PersistentState =
  { aggregatorAddr :: Maybe (LocalOrRemote Server)
  }

type StateServerName = PersistentInstanceState.StateServerName PersistentState

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
    , stateServerName :: StateServerName
    }

type StartArgs
  = { streamPublish :: StreamPublish
    , streamDetails :: StreamDetails
    , ingestKey :: IngestKey
    , remoteAddress :: String
    , remotePort :: Int
    , handlerPid :: Pid
    }

startLink :: StartArgs -> StateServerName -> Effect StartLinkResult
startLink args@{ingestKey} stateServerName = Gen.startLink (serverName ingestKey) (init args stateServerName) handleInfo

stopAction :: IngestKey -> Maybe PersistentState -> Effect Unit
stopAction ingestKey Nothing = pure unit
stopAction ingestKey (Just {aggregatorAddr: Nothing}) = pure unit
stopAction ingestKey (Just {aggregatorAddr: Just addr}) =
  removeIngest addr
  where
    removeIngest (Local aggregator) = do
      IngestAggregatorInstance.removeLocalIngest ingestKey
      pure unit
    removeIngest (Remote aggregator) = do
      let
        url = makeActiveIngestUrl aggregator ingestKey
      void $ crashIfLeft =<< SpudGun.delete url {}

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

init :: StartArgs -> StateServerName -> Effect State
init { streamPublish
     , streamDetails
     , ingestKey
     , remoteAddress
     , remotePort
     , handlerPid} stateServerName = do

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
       , stateServerName
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
ingestEqLine state@{ ingestKey: ingestKey@(IngestKey slotId slotRole _profileeName)
                   , streamPublish: StreamPublish { host: ingestIp
                                                  , protocol: connectionType
                                                  , rtmpShortName
                                                  , rtmpStreamName
                                                  , username }
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
       , rtmpShortName
       , rtmpStreamName
       , slotId
       , slotRole
       , connectionType
       , startMs
       , endMs
       , bytesWritten: totalBytesSent
       , bytesRead: totalBytesReceived
       , lostPackets: 0
       }

doStopIngest :: State -> Effect Unit
doStopIngest state@{aggregatorAddr, ingestKey} = do
  -- Happy state exit - we also need to remove ourselves from the ingest aggregator, but that is
  -- done in the stopAction, which will also get called in the unhappy case
  eqLine <- ingestEqLine state
  Audit.ingestStop eqLine
  pure unit

informAggregator :: State -> Effect State
informAggregator state@{streamDetails, ingestKey, thisServer, aggregatorRetryTime, stateServerName} = do
  maybeAggregator <- hush <$> getAggregator
  maybeIngestAdded <- sequence (addIngest <$> (extractServer <$>  maybeAggregator))
  case fromMaybe false maybeIngestAdded of
    true -> do
      PersistentInstanceState.recordInstanceData stateServerName {aggregatorAddr: maybeAggregator}
      pure state{aggregatorAddr = maybeAggregator}
    false -> do
      void $ Timer.sendAfter (serverName ingestKey) (unwrap aggregatorRetryTime) InformAggregator
      pure state
  where
    addIngest :: Server -> Effect Boolean
    addIngest aggregatorAddress
      | aggregatorAddress == thisServer = do
        IngestAggregatorInstance.addLocalIngest ingestKey
        pure true
      | otherwise = do
        let
          -- TODO - functions to make URLs from Server
          url = makeActiveIngestUrl aggregatorAddress ingestKey
        restResult <- SpudGun.postJson url $ extractAddress thisServer
        case restResult of
          Left _ -> pure $ false
          Right _ -> pure $ true

    getAggregator :: Effect (ResourceResp Server)
    getAggregator = do
      maybeAggregator <- IntraPoP.whereIsIngestAggregator (ingestKeyToAggregatorKey ingestKey)
      case maybeAggregator of
        Just server ->
          pure $ Right $ Local server
        Nothing ->
          launchLocalOrRemote

    launchLocalOrRemote :: Effect (ResourceResp Server)
    launchLocalOrRemote = do
      launchLocalOrRemoteGeneric filterForAggregatorLoad launchLocal launchRemote
      where
        launchLocal :: ServerLoad -> Effect Unit
        launchLocal _ = do
          void $ IngestAggregatorSup.startAggregator streamDetails
          pure unit
        launchRemote idleServer = do
          let
            url = makeUrl idleServer IngestAggregatorsE
          void $ crashIfLeft =<< SpudGun.postJson url streamDetails

handleAggregatorExit :: AggregatorKey -> Server -> State -> Effect State
handleAggregatorExit exitedAggregatorKey exitedAggregatorAddr state@{ingestKey, aggregatorRetryTime, aggregatorAddr}
  | exitedAggregatorKey == (ingestKeyToAggregatorKey ingestKey) && Just exitedAggregatorAddr == (extractServer <$> aggregatorAddr) = do
      void $ Timer.sendAfter (serverName ingestKey) 0 InformAggregator
      pure state
  | otherwise =
      pure state

makeActiveIngestUrl :: Server -> IngestKey -> Url
makeActiveIngestUrl server (IngestKey slotId streamRole profileName) =
  makeUrl server $ IngestAggregatorActiveIngestsE slotId streamRole profileName

loadThresholdToCreateAggregator :: Load
loadThresholdToCreateAggregator = wrap 50.0

filterForAggregatorLoad :: ServerLoad -> Boolean
filterForAggregatorLoad (ServerLoad sl) = sl.load < loadThresholdToCreateAggregator

--------------------------------------------------------------------------------
-- Log helpers
--------------------------------------------------------------------------------
domain :: List Atom
domain = atom <$> (show Agent.Ingest :  "Instance" : nil)

logInfo :: forall a. Logger a
logInfo = domainLog Logger.info

--logWarning :: forall a. Logger a
--logWarning = domainLog Logger.warning

domainLog :: forall a. Logger {domain :: List Atom, misc :: a} -> Logger a
domainLog = Logger.doLog domain
