module Rtsv2.Agents.IngestInstance
   ( startLink
   , stopAction
   , isActive
   , setClientMetadata
   , setSourceInfo
   , getPublicState
   , dataObjectSendMessage
   , dataObjectUpdate
   , stopIngest
   , getStreamAuthType
   , getPublishCredentials
   , getStreamDetails

   , StartArgs
   , CachedState
   , WebSocket
   , StateServerName
   , domain
) where

import Prelude

import Bus as Bus
import Data.Either (Either(..), either, hush)
import Data.Int (round)
import Data.Long as Long
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (unwrap, wrap)
import Effect (Effect)
import Erl.Atom (Atom, atom)
import Erl.Data.List (List, nil, (:))
import Erl.Data.Tuple (Tuple2, tuple2)
import Erl.Process.Raw (Pid)
import Erl.Utils (systemTimeMs)
import Logger as Logger
import Pinto (ServerName, StartLinkResult)
import Pinto as Pinto
import Pinto.Gen (CallResult(..), CastResult(..), TerminateReason(..))
import Pinto.Gen as Gen
import Pinto.Timer as Timer
import Prim.Row as Row
import Rtsv2.Agents.CachedInstanceState as CachedInstanceState
import Rtsv2.Agents.IngestAggregatorSup as IngestAggregatorSup
import Rtsv2.Agents.IngestStats as IngestStats
import Rtsv2.Agents.IntraPoP (IntraPoPBusMessage(..))
import Rtsv2.Agents.IntraPoP as IntraPoP
import Rtsv2.Agents.StreamRelayTypes (AggregatorToIngestWsMessage(..), IngestToAggregatorWsMessage(..))
import Rtsv2.Audit as Audit
import Rtsv2.Config as Config
import Rtsv2.DataObject as DO
import Rtsv2.LlnwApi as LlnwApi
import Rtsv2.Names as Names
import Rtsv2.PoPDefinition as PoPDefinition
import Rtsv2.Types (fromLocalOrRemote)
import Shared.Common (LoggingContext(..), Milliseconds(..))
import Shared.Rtsv2.Agent as Agent
import Shared.Rtsv2.Agent.State as PublicState
import Shared.Rtsv2.JsonLd as JsonLd
import Shared.Rtsv2.LlnwApiTypes (AuthType, PublishCredentials, StreamAuth, StreamConnection, StreamDetails, StreamIngestProtocol(..), StreamPublish(..))
import Shared.Rtsv2.Router.Endpoint.System as System
import Shared.Rtsv2.Stream (AggregatorKey, IngestKey(..), ingestKeyToAggregatorKey)
import Shared.Rtsv2.Types (OnBehalfOf(..), Server, extractAddress)
import Shared.Types.Media.Types.Rtmp (RtmpClientMetadata)
import Shared.Types.Media.Types.SourceDetails (SourceInfo)
import WsGun as WsGun

serverName :: IngestKey -> ServerName State Msg
serverName ingestKey = Names.ingestInstanceName ingestKey

data Msg
   = WriteEqLog
   | InformAggregator
   | IntraPoPBus IntraPoP.IntraPoPBusMessage
   | HandlerDown
   | Gun WsGun.GunMsg

data IngestBusMsg = IngestDataObjectMessage DO.Message
                  | IngestDataObjectUpdateResponse DO.ObjectUpdateResponseMessage
                  | IngestDataObjectBroadcast DO.Object

bus :: IngestKey -> Bus.Bus (Tuple2 Atom IngestKey) IngestBusMsg
bus ingestKey = Bus.bus (tuple2 (atom "ingestBus") ingestKey)

type WebSocket = WsGun.WebSocket IngestToAggregatorWsMessage AggregatorToIngestWsMessage

type CachedState = WebSocket

type StateServerName = CachedInstanceState.StateServerName CachedState

type State
  = { thisServer :: Server
    , aggregatorRetryTime :: Milliseconds
    , ingestKey :: IngestKey
    , loadConfig :: Config.LoadConfig
    , streamPublish :: StreamPublish
    , streamDetails :: StreamDetails
    , ingestStartedTime :: Milliseconds
    , lastIngestAuditTime :: Milliseconds
    , remoteAddress :: String
    , remotePort :: Int
    , localPort :: Int
    , aggregatorWebSocket :: Maybe WebSocket
    , aggregatorServer :: Maybe Server
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

------------------------------------------------------------------------------
-- API
------------------------------------------------------------------------------
startLink :: StartArgs -> StateServerName -> Effect StartLinkResult
startLink args@{ingestKey} stateServerName = Gen.startLink (serverName ingestKey) (init args stateServerName) handleInfo

getStreamAuthType :: String -> String -> String -> Effect (Maybe AuthType)
getStreamAuthType host rtmpShortName clientIp = do
  config <- Config.llnwApiConfig
  restResult <- LlnwApi.streamAuthType config (wrap { host
                                                    , protocol: Rtmp
                                                    , rtmpShortName: wrap rtmpShortName
                                                    , clientIp} :: StreamConnection)
  either error (pure <<< Just) restResult
  where
    error err = do
      logInfoWithMetadata "StreamAuthType error" {alertId: (atom "lsrsFailure")} {reason: err}
      pure Nothing

getPublishCredentials :: String -> String -> String -> String -> Effect (Maybe PublishCredentials)
getPublishCredentials host rtmpShortName username clientIp = do
  config <- Config.llnwApiConfig
  restResult <- LlnwApi.streamAuth config (wrap { host
                                                , rtmpShortName: wrap rtmpShortName
                                                , username
                                                , clientIp} :: StreamAuth)
  either error (pure <<< Just) restResult
  where
    error err = do
      logInfoWithMetadata "StreamAuth error" {alertId: (atom "lsrsFailure")} {reason: err}
      pure Nothing

getStreamDetails :: StreamPublish -> Effect (Maybe StreamDetails)
getStreamDetails streamPublish = do
  config <- Config.llnwApiConfig
  restResult <- LlnwApi.streamPublish config streamPublish
  either error (pure <<< Just) restResult
  where
    error err = do
      logInfoWithMetadata "StreamPublish error" {alertId: (atom "lsrsFailure")} {reason: err}
      pure Nothing

stopAction :: IngestKey -> Maybe CachedState -> Effect Unit
stopAction ingestKey mCachedState = do
  logStop "Ingest stopping" {ingestKey}
  fromMaybe (pure unit) $ WsGun.closeWebSocket <$> mCachedState
  pure unit

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
    logInfo "Stopping due to stopIngest called" {ingestKey}
    doStopIngest state
    pure $ CallStop unit state

getPublicState :: IngestKey -> Effect (PublicState.Ingest List)
getPublicState ingestKey@(IngestKey slotId slotRole profileName) =
  Gen.doCall (serverName ingestKey) \state@{ thisServer
                                           , clientMetadata
                                           , sourceInfo
                                           , remoteAddress
                                           , remotePort
                                           , ingestStartedTime} -> do
    let
      publicState = { rtmpClientMetadata: clientMetadata
                    , sourceInfo
                    , remoteAddress
                    , remotePort
                    , ingestStartedTime }
    node <- JsonLd.ingestStateNode slotId slotRole profileName publicState thisServer
    pure $ CallReply node state

dataObjectSendMessage :: IngestKey -> DO.Message -> Effect Unit
dataObjectSendMessage ingestKey msg =
  Gen.doCall (serverName ingestKey)
  (\state@{aggregatorWebSocket} -> do
      case aggregatorWebSocket of
        Just socket -> void $ WsGun.send socket (IngestToAggregatorDataObjectMessage msg)
        Nothing -> pure unit
      pure $ CallReply unit state
  )

dataObjectUpdate :: IngestKey -> DO.ObjectUpdateMessage -> Effect Unit
dataObjectUpdate ingestKey updateMsg =
  Gen.doCall (serverName ingestKey)
  (\state@{aggregatorWebSocket} -> do
      case aggregatorWebSocket of
        Just socket -> void $ WsGun.send socket (IngestToAggregatorDataObjectUpdateMessage updateMsg)
        Nothing -> pure unit
      pure $ CallReply unit state
  )

------------------------------------------------------------------------------
-- gen-server callbacks
------------------------------------------------------------------------------
init :: StartArgs -> StateServerName -> Effect State
init { streamPublish
     , streamDetails: streamDetails@{slot: {name: slotName}}
     , ingestKey: ingestKey@(IngestKey slotId slotRole profileName)
     , remoteAddress
     , remotePort
     , handlerPid} stateServerName = do
  Logger.addLoggerContext $ PerProfile { slotId, slotRole, slotName, profileName}

  logStart "Ingest starting" {ingestKey, handlerPid}
  loadConfig <- Config.loadConfig
  thisServer <- PoPDefinition.getThisServer
  now <- systemTimeMs
  {port: localPort} <- Config.rtmpIngestConfig
  {eqLogIntervalMs, aggregatorRetryTimeMs} <- Config.ingestInstanceConfig

  Gen.monitorPid ourServerName handlerPid (\_ -> HandlerDown)
  void $ Bus.subscribe ourServerName IntraPoP.bus IntraPoPBus
  void $ Timer.sendAfter ourServerName 0 InformAggregator
  void $ Timer.sendEvery ourServerName eqLogIntervalMs WriteEqLog
  Gen.registerExternalMapping (serverName ingestKey) (\m -> Gun <$> (WsGun.messageMapper m))
  Gen.registerTerminate (serverName ingestKey) terminate

  pure { thisServer
       , streamPublish
       , streamDetails
       , loadConfig
       , ingestKey
       , aggregatorRetryTime: Milliseconds $ Long.fromInt aggregatorRetryTimeMs
       , aggregatorWebSocket: Nothing
       , aggregatorServer: Nothing
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

  IntraPoPBus (VmReset _ _ _) ->
    pure $ CastNoReply state

  IntraPoPBus (IngestAggregatorStarted _ _) -> do
    pure $ CastNoReply state

  IntraPoPBus (IngestAggregatorExited aggregatorKey serverAddress) -> do
    state2 <- handleAggregatorExit aggregatorKey serverAddress state
    pure $ CastNoReply state2

  IntraPoPBus (StreamRelayStarted _ _) ->
    pure $ CastNoReply state

  IntraPoPBus (StreamRelayExited _ _) ->
    pure $ CastNoReply state

  IntraPoPBus (EgestStarted _ _) ->
    pure $ CastNoReply state

  IntraPoPBus (EgestExited _ _) ->
    pure $ CastNoReply state

  HandlerDown -> do
    logInfo "Ingest Handler has exited" {ingestKey}
    doStopIngest state
    pure $ CastStop state

  Gun gunMsg ->
    processGunMessage state gunMsg

terminate :: TerminateReason -> State -> Effect Unit
terminate Normal state = do
  logInfo "IngestInstance terminating" {reason: Normal}
  pure unit
terminate reason state = do
  logInfo "IngestInstance terminating" {reason}
  eqLine <- ingestEqLine state
  Audit.ingestStop eqLine
  pure unit

------------------------------------------------------------------------------
-- Internals
------------------------------------------------------------------------------
processGunMessage :: State -> WsGun.GunMsg -> Effect (CastResult State)
processGunMessage state@{aggregatorWebSocket: Nothing} gunMsg =
  pure $ CastNoReply state

processGunMessage state@{aggregatorWebSocket: Just socket, ingestKey} gunMsg =
  if WsGun.isSocketForMessage gunMsg socket then do
    processResponse <- WsGun.processMessage socket gunMsg
    case processResponse of
      Left error -> do
        logInfo "Gun process error" {error}
        pure $ CastNoReply state

      Right (WsGun.Internal _) ->
        pure $ CastNoReply state

      Right (WsGun.WebSocketUpdate newSocket) ->
        pure $ CastNoReply state{aggregatorWebSocket = Just newSocket}

      Right WsGun.WebSocketUp -> do
        logInfo "Aggregator WebSocket up" {}
        pure $ CastNoReply state

      Right WsGun.WebSocketDown -> do
        logInfo "Aggregator WebSocket down" {}
        state2 <- informAggregator state
        pure $ CastNoReply state2

      Right (WsGun.Frame IngestStop) -> do
        logInfo "Aggregator requested that ingest stops" {}
        pure $ CastStop state

      Right (WsGun.Frame (AggregatorToIngestDataObjectMessage msg)) -> do
        Bus.raise (bus ingestKey) (IngestDataObjectMessage msg)
        pure $ CastNoReply state

      Right (WsGun.Frame (AggregatorToIngestDataObject object)) -> do
        Bus.raise (bus ingestKey) (IngestDataObjectBroadcast object)
        pure $ CastNoReply state

      Right (WsGun.Frame (AggregatorToIngestDataObjectUpdateResponse response)) -> do
        Bus.raise (bus ingestKey) (IngestDataObjectUpdateResponse response)
        pure $ CastNoReply state

  else
    pure $ CastNoReply state

ingestEqLine :: State -> Effect Audit.IngestEqLine
ingestEqLine state@{ ingestKey: ingestKey@(IngestKey slotId slotRole _profileName)
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

informAggregator :: State -> Effect State
informAggregator state@{ streamDetails
                       , streamPublish: StreamPublish {rtmpShortName}
                       , ingestKey: ingestKey@(IngestKey slotId slotRole profileName)
                       , thisServer
                       , aggregatorRetryTime
                       , stateServerName
                       , loadConfig} =
  join $ maybe failure success <$> (addIngest =<< getAggregator)

  where
    success {aggregator, socket} = do
      logInfo "WebSocket connection started" {aggregator}
      CachedInstanceState.recordInstanceData stateServerName socket
      pure $ state{ aggregatorWebSocket = Just socket
                  , aggregatorServer = Just aggregator}

    failure = do
      logInfo "WebSocket attempt failed; retrying" {aggregatorRetryTime}
      void $ Timer.sendAfter (serverName ingestKey) (round $ Long.toNumber $ unwrap aggregatorRetryTime) InformAggregator
      pure $ state

    addIngest Nothing = pure Nothing
    addIngest (Just aggregator) = do
      wsUrl <- System.makeWsUrl aggregator $ System.IngestAggregatorRegisteredIngestWs slotId slotRole profileName (extractAddress thisServer)
      webSocket <- WsGun.openWebSocket (serverName ingestKey) Gun wsUrl
      pure $ {aggregator, socket: _} <$> hush webSocket

    getAggregator = do
      maybeAggregator <- IntraPoP.whereIsIngestAggregator (ingestKeyToAggregatorKey ingestKey)
      case maybeAggregator of
        Just server ->
          pure $ Just server
        Nothing -> do
          let
            payload = { shortName: rtmpShortName
                      , streamDetails
                      , dataObject: Nothing}
          (map fromLocalOrRemote) <$> hush <$> IngestAggregatorSup.startLocalOrRemoteAggregator loadConfig LocalAgent payload

handleAggregatorExit :: AggregatorKey -> Server -> State -> Effect State
handleAggregatorExit exitedAggregatorKey exitedAggregator state@{ ingestKey
                                                                , aggregatorWebSocket: mWebSocket
                                                                , aggregatorServer: mAggregatorServer
                                                                , aggregatorRetryTime}

  | exitedAggregatorKey == (ingestKeyToAggregatorKey ingestKey) && Just exitedAggregator == mAggregatorServer = do
    logInfo "Aggregator has exited" {exitedAggregatorKey, exitedAggregator, ingestKey: state.ingestKey}
    fromMaybe (pure unit) $ WsGun.closeWebSocket <$> mWebSocket
    void $ Timer.sendAfter (serverName ingestKey) (round $ Long.toNumber $ unwrap aggregatorRetryTime) InformAggregator
    pure state{ aggregatorWebSocket = Nothing
              , aggregatorServer = Nothing}
  | otherwise = do
    logInfo "Some other aggregator gone" { exitedAggregatorKey
                                         , exitedAggregator
                                         , ingestKey
                                         , mAggregatorServer}
    pure state

--------------------------------------------------------------------------------
-- Log helpers
--------------------------------------------------------------------------------
domain :: List Atom
domain = atom <$> (show Agent.Ingest :  "Instance" : nil)

logInfo :: forall report. String -> { | report } -> Effect Unit
logInfo msg = Logger.info (Logger.traceMetadata domain msg)

logInfoWithMetadata :: forall report metadata.
                       Row.Lacks "domain" metadata =>
                       Row.Lacks "type" metadata =>
                       Row.Lacks "text" metadata =>
                       String -> { | metadata } -> { | report } -> Effect Unit
logInfoWithMetadata msg = Logger.info <<< Logger.genericMetadata domain msg

logStart :: forall report. String -> { | report } -> Effect Unit
logStart = Logger.info <<< Logger.eventMetadata domain Logger.Start

logStop :: forall report. String -> { | report } -> Effect Unit
logStop = Logger.info <<< Logger.eventMetadata domain Logger.Stop
