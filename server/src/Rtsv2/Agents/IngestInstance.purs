module Rtsv2.Agents.IngestInstance
   ( startLink
   , stopAction
   , isActive
   , setClientMetadata
   , setSourceInfo
   , getPublicState
   , stopIngest
   , StartArgs

   , CachedState
   , StateServerName
   , domain
) where

import Prelude

import Bus as Bus
import Data.Either (Either(..), hush)
import Data.Int (round, toNumber)
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
import Rtsv2.Agents.CachedInstanceState as CachedInstanceState
import Rtsv2.Agents.IngestAggregatorInstance (RemoteIngestPayload)
import Rtsv2.Agents.IngestAggregatorInstance as IngestAggregatorInstance
import Rtsv2.Agents.IngestAggregatorSup as IngestAggregatorSup
import Rtsv2.Agents.IngestStats as IngestStats
import Rtsv2.Agents.IntraPoP (IntraPoPBusMessage(..), launchLocalOrRemoteGeneric)
import Rtsv2.Agents.IntraPoP as IntraPoP
import Rtsv2.Agents.Locator (extractServer)
import Rtsv2.Agents.Locator.Types (LocalOrRemote(..), ResourceResp)
import Rtsv2.Audit as Audit
import Rtsv2.Config as Config
import Rtsv2.Names as Names
import Rtsv2.PoPDefinition as PoPDefinition
import Rtsv2.Utils (crashIfLeft)
import Shared.Agent as Agent
import Shared.Common (Milliseconds, Url)
import Shared.LlnwApiTypes (StreamDetails, StreamPublish(..))
import Shared.Router.Endpoint (Endpoint(..), makeUrl)
import Shared.Rtsv2.JsonLd as JsonLd
import Shared.Stream (AggregatorKey, IngestKey(..), ingestKeyToAggregatorKey)
import Shared.Types (Load, Server, ServerLoad(..))
import Shared.Types.Agent.State as PublicState
import Shared.Types.Media.Types.Rtmp (RtmpClientMetadata)
import Shared.Types.Media.Types.SourceDetails (SourceInfo)
import SpudGun (SpudError(..))
import SpudGun as SpudGun

serverName :: IngestKey -> ServerName State Msg
serverName ingestKey = Names.ingestInstanceName ingestKey

data Msg
   = WriteEqLog
   | InformAggregator
   | IntraPoPBus IntraPoP.IntraPoPBusMessage
   | HandlerDown

type CachedState =
  { aggregatorAddr :: Maybe (LocalOrRemote Server)
  }

type StateServerName = CachedInstanceState.StateServerName CachedState

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
    , cachedState :: CachedState
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

stopAction :: IngestKey -> Maybe CachedState -> Effect Unit
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
getPublicState ingestKey@(IngestKey slotId slotRole profileName) =
  Gen.doCall (serverName ingestKey) \state@{ thisServer
                                           , clientMetadata
                                           , sourceInfo
                                           , remoteAddress
                                           , remotePort
                                           , ingestStartedTime} ->
    let
      publicState = { rtmpClientMetadata: clientMetadata
                    , sourceInfo
                    , remoteAddress
                    , remotePort
                    , ingestStartedTime }
      node = JsonLd.ingestStateNode slotId slotRole profileName publicState thisServer
    in
     pure $ CallReply node state

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
       , aggregatorRetryTime: wrap $ toNumber intraPoPLatencyMs
       , cachedState: {aggregatorAddr: Nothing}
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
    result <- informAggregator state
    case result of
      Right state2 ->
        pure $ CastNoReply state2
      Left unit -> do
        logInfo "Aggregator did not allow ingest to register; stopping ingest" {ingestKey}
        pure $ CastStop state

  IntraPoPBus (VmReset _ _ _) ->
    pure $ CastNoReply state

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
doStopIngest state@{ingestKey} = do
  -- Happy state exit - we also need to remove ourselves from the ingest aggregator, but that is
  -- done in the stopAction, which will also get called in the unhappy case
  eqLine <- ingestEqLine state
  Audit.ingestStop eqLine
  pure unit

informAggregator :: State -> Effect (Either Unit State)
informAggregator state@{streamDetails, ingestKey, thisServer, aggregatorRetryTime, stateServerName} = do
  maybeAggregator <- hush <$> getAggregator
  maybeIngestAdded <- sequence (addIngest <$> (extractServer <$>  maybeAggregator))
  case fromMaybe (Right false) maybeIngestAdded of
    Right true -> do
      let
        cachedState = {aggregatorAddr: maybeAggregator}
      CachedInstanceState.recordInstanceData stateServerName cachedState
      pure $ Right state{cachedState = cachedState}
    Right false -> do
      void $ Timer.sendAfter (serverName ingestKey) (round $ unwrap aggregatorRetryTime) InformAggregator
      pure $ Right state
    Left unit -> do
      pure $ Left unit
  where
    addIngest :: Server -> Effect (Either Unit Boolean)
    addIngest aggregatorAddress
      | aggregatorAddress == thisServer = do
        result <- IngestAggregatorInstance.addLocalIngest ingestKey
        pure $ case result of
                 true -> Right true
                 false -> Left unit
      | otherwise = do
        let
          -- TODO - functions to make URLs from Server
          url = makeActiveIngestUrl aggregatorAddress ingestKey
        vmRef <- IntraPoP.currentLocalRef
        restResult <- SpudGun.postJson url $ ({ ingestAddress: thisServer
                                              , vmRef: vmRef} :: RemoteIngestPayload)
        case restResult of
          Left (RequestError _) -> pure $ Right false -- We treat network errors etc as transitory and try again
          Left (ResponseError response) -> do
            logInfo "Failed to register with aggregator; exiting" {ingestKey, aggregatorAddress, response}
            pure $ Left unit
          Right _ ->
            pure $ Right true

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
handleAggregatorExit exitedAggregatorKey exitedAggregatorAddr state@{ingestKey, aggregatorRetryTime, cachedState: {aggregatorAddr}}
  | exitedAggregatorKey == (ingestKeyToAggregatorKey ingestKey) && Just exitedAggregatorAddr == (extractServer <$> aggregatorAddr) = do
      void $ Timer.sendAfter (serverName ingestKey) 0 InformAggregator
      pure state
  | otherwise =
      pure state

makeActiveIngestUrl :: Server -> IngestKey -> Url
makeActiveIngestUrl server (IngestKey slotId slotRole profileName) =
  makeUrl server $ IngestAggregatorActiveIngestsE slotId slotRole profileName

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
