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
   , StartArgs

   , CachedState
   , WebSocket
   , StateServerName
   , domain
) where

import Prelude

import Bus as Bus
import Data.Either (Either(..), hush)
import Data.Int (round, toNumber)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap, wrap)
import Effect (Effect)
import Erl.Atom (Atom, atom)
import Erl.Data.List (List, nil, (:))
import Erl.Data.Tuple (Tuple2, tuple2)
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
import Rtsv2.Agents.IngestAggregatorSup as IngestAggregatorSup
import Rtsv2.Agents.IngestStats as IngestStats
import Rtsv2.Agents.IntraPoP (IntraPoPBusMessage(..))
import Rtsv2.Agents.IntraPoP as IntraPoP
import Rtsv2.Agents.StreamRelayTypes (AggregatorToIngestWsMessage(..), IngestToAggregatorWsMessage(..))
import Rtsv2.Audit as Audit
import Rtsv2.Config as Config
import Rtsv2.DataObject as DO
import Rtsv2.Names as Names
import Rtsv2.PoPDefinition as PoPDefinition
import Shared.Common (Milliseconds)
import Shared.Rtsv2.Agent as Agent
import Shared.Rtsv2.Agent.State as PublicState
import Shared.Rtsv2.JsonLd as JsonLd
import Shared.Rtsv2.LlnwApiTypes (StreamDetails, StreamPublish(..))
import Shared.Rtsv2.Router.Endpoint.System as System
import Shared.Rtsv2.Stream (AggregatorKey, IngestKey(..), ingestKeyToAggregatorKey)
import Shared.Rtsv2.Types (Canary, LocalOrRemote(..), ResourceResp, Server, extractAddress, fromLocalOrRemote)
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
    , clientMetadata :: Maybe (RtmpClientMetadata List)
    , sourceInfo :: Maybe (SourceInfo List)
    , stateServerName :: StateServerName
    }

type StartArgs
  = { streamPublish :: StreamPublish
    , streamDetails :: StreamDetails
    , ingestKey :: IngestKey
    , canary :: Canary
    , remoteAddress :: String
    , remotePort :: Int
    , handlerPid :: Pid
    }

startLink :: StartArgs -> StateServerName -> Effect StartLinkResult
startLink args@{ingestKey} stateServerName = Gen.startLink (serverName ingestKey) (init args stateServerName) handleInfo

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
      _ <- case aggregatorWebSocket of
        Just socket -> void $ WsGun.send socket (IngestToAggregatorDataObjectMessage msg)
        Nothing -> pure unit
      pure $ CallReply unit state
  )

dataObjectUpdate :: IngestKey -> DO.ObjectUpdateMessage -> Effect Unit
dataObjectUpdate ingestKey updateMsg =
  Gen.doCall (serverName ingestKey)
  (\state@{aggregatorWebSocket} -> do
      _ <- case aggregatorWebSocket of
        Just socket -> void $ WsGun.send socket (IngestToAggregatorDataObjectUpdateMessage updateMsg)
        Nothing -> pure unit
      pure $ CallReply unit state
  )

init :: StartArgs -> StateServerName -> Effect State
init { streamPublish
     , streamDetails
     , ingestKey
     , remoteAddress
     , remotePort
     , handlerPid} stateServerName = do

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

  pure { thisServer
       , streamPublish
       , streamDetails
       , loadConfig
       , ingestKey
       , aggregatorRetryTime: wrap $ toNumber aggregatorRetryTimeMs
       , aggregatorWebSocket: Nothing
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

  HandlerDown -> do
    logInfo "Ingest Handler has exited" {ingestKey}
    doStopIngest state
    pure $ CastStop state

  Gun gunMsg ->
    processGunMessage state gunMsg

processGunMessage :: State -> WsGun.GunMsg -> Effect (CastResult State)
processGunMessage state@{aggregatorWebSocket: Nothing} gunMsg =
  pure $ CastNoReply state

processGunMessage state@{aggregatorWebSocket: Just socket, ingestKey} gunMsg =
  if WsGun.isSocketForMessage gunMsg socket then do
    processResponse <- WsGun.processMessage socket gunMsg
    case processResponse of
      Left error -> do
        _ <- logInfo "Gun process error" {error}
        pure $ CastNoReply state

      Right (WsGun.Internal _) ->
        pure $ CastNoReply state

      Right WsGun.WebSocketUp -> do
        _ <- logInfo "Aggregator WebSocket up" {}
        pure $ CastNoReply state

      Right WsGun.WebSocketDown -> do
        _ <- logInfo "Aggregator WebSocket down" {}
        state2 <- informAggregator state
        pure $ CastNoReply state2

      Right (WsGun.Frame IngestStop) -> do
        _ <- logInfo "Aggregator requested that ingest stops" {}
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
                       , loadConfig} = do
  maybeAggregator <- hush <$> getAggregator
  maybeIngestAdded <- addIngest $ (fromLocalOrRemote <$> maybeAggregator)
  case maybeIngestAdded of
    Just webSocket -> do
      logInfo "WebSocket connection started" {maybeAggregator}
      CachedInstanceState.recordInstanceData stateServerName webSocket
      pure $ state{aggregatorWebSocket = Just webSocket}
    Nothing -> do
      logInfo "WebSocket attempt failed; retrying" {maybeAggregator, aggregatorRetryTime}
      void $ Timer.sendAfter (serverName ingestKey) (round $ unwrap aggregatorRetryTime) InformAggregator
      pure $ state
  where
    addIngest :: Maybe Server -> Effect (Maybe WebSocket)
    addIngest Nothing = pure Nothing
    addIngest (Just aggregatorAddress) = do
        wsUrl <- System.makeWsUrl aggregatorAddress $ System.IngestAggregatorRegisteredIngestWs slotId slotRole profileName (extractAddress thisServer)
        webSocket <- WsGun.openWebSocket wsUrl
        pure $ hush webSocket

    getAggregator :: Effect (ResourceResp Server)
    getAggregator = do
      maybeAggregator <- IntraPoP.whereIsIngestAggregator (ingestKeyToAggregatorKey ingestKey)
      case maybeAggregator of
        Just server ->
          pure $ Right $ Local server
        Nothing ->
          IngestAggregatorSup.startLocalOrRemoteAggregator loadConfig {shortName: rtmpShortName, streamDetails}

handleAggregatorExit :: AggregatorKey -> Server -> State -> Effect State
handleAggregatorExit exitedAggregatorKey exitedAggregatorAddr state@{ingestKey, aggregatorRetryTime, aggregatorWebSocket: mWebSocket}
  | exitedAggregatorKey == (ingestKeyToAggregatorKey ingestKey) = do
      logInfo "Aggregator has exited" {exitedAggregatorKey, exitedAggregatorAddr, ingestKey: state.ingestKey}
      fromMaybe (pure unit) $ WsGun.closeWebSocket <$> mWebSocket
      void $ Timer.sendAfter (serverName ingestKey) 0 InformAggregator
      pure state
  | otherwise =
      pure state

--------------------------------------------------------------------------------
-- Log helpers
--------------------------------------------------------------------------------
domain :: List Atom
domain = atom <$> (show Agent.Ingest :  "Instance" : nil)

logInfo :: forall a. Logger (Record a)
logInfo = Logger.doLog domain Logger.info

logStart :: forall a. Logger (Record a)
logStart = Logger.doLogEvent domain Logger.Start Logger.info

logStop :: forall a. Logger (Record a)
logStop = Logger.doLogEvent domain Logger.Stop Logger.info
