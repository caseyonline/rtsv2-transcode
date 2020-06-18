module Rtsv2.Agents.EgestInstance
  ( startLink
  , stopAction
  , isActive
  , pendingClient
  , addClient
  , currentStats
  , getSlotConfiguration
  , dataObjectSendMessage
  , dataObjectUpdate
  , statsUpdate
  , forceDrain
  , CreateEgestPayload

  , CachedState
  , ParentCallbacks
  , WebSocket
  , StateServerName
  , RegistrationResp
  , domain
) where

import Prelude

import Bus as Bus
import Data.Either (Either(..), hush)
import Data.Foldable (foldl)
import Data.Int (round, toNumber)
import Data.Long as Long
import Data.Maybe (Maybe(..), fromMaybe, maybe')
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Traversable (traverse_)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Erl.Atom (Atom, atom)
import Erl.Data.List (List, nil, singleton)
import Erl.Data.List as List
import Erl.Data.Map (Map, lookup, toUnfoldable, values)
import Erl.Data.Map as Map
import Erl.Data.Tuple (Tuple2, Tuple3, tuple2)
import Erl.Data.Tuple as Tuple
import Erl.Process (Process(..), (!))
import Erl.Process.Raw (Pid)
import Erl.Utils (Ref, makeRef, systemTimeMs)
import Erl.Utils as Erl
import Logger as Logger
import Pinto (ServerName, StartLinkResult)
import Pinto as Pinto
import Pinto.Gen (CallResult(..), CastResult(..), TerminateReason(..))
import Pinto.Gen as Gen
import Pinto.Timer as Timer
import Rtsv2.Agents.CachedInstanceState as CachedInstanceState
import Rtsv2.Agents.EgestInstance.WebRTCTypes (WebRTCStreamServerStats, WebRTCSessionManagerStats)
import Rtsv2.Agents.IntraPoP (IntraPoPBusMessage(..), announceLocalEgestStopped)
import Rtsv2.Agents.IntraPoP as IntraPoP
import Rtsv2.Agents.SlotTypes (SlotConfiguration)
import Rtsv2.Agents.StreamRelayTypes (ActiveProfiles(..), DownstreamWsMessage(..), EgestUpstreamWsMessage(..), NativeJson, CreateRelayPayload)
import Rtsv2.Audit as Audit
import Rtsv2.Config (LoadConfig, MediaGatewayFlag)
import Rtsv2.Config as Config
import Rtsv2.DataObject (ObjectBroadcastMessage(..))
import Rtsv2.DataObject as DO
import Rtsv2.DataObject as DataObject
import Rtsv2.Env as Env
import Rtsv2.Load as Load
import Rtsv2.LoadTypes (LoadFixedCost(..), PredictedLoad(..))
import Rtsv2.Names as Names
import Rtsv2.PoPDefinition as PoPDefinition
import Rtsv2.Types (LocalOrRemote(..), ResourceResp)
import Shared.Common (LoggingContext(..), Milliseconds(..))
import Shared.Rtsv2.Agent (SlotCharacteristics)
import Shared.Rtsv2.Agent as Agent
import Shared.Rtsv2.JsonLd (EgestStats, EgestSessionStats)
import Shared.Rtsv2.LlnwApiTypes (StreamIngestProtocol(..))
import Shared.Rtsv2.Router.Endpoint.System as System
import Shared.Rtsv2.Stream (AggregatorKey(..), EgestKey(..), ProfileName, RelayKey(..), SlotId, SlotRole, egestKeyToAgentKey, egestKeyToAggregatorKey)
import Shared.Rtsv2.Types (EgestServer, FailureReason(..), OnBehalfOf(..), PoPName, RelayServer, Server(..), extractAddress)
import WsGun as WsGun

foreign import startEgestReceiverFFI :: EgestKey -> MediaGatewayFlag -> Effect (Tuple3 Int Int WorkflowHandle)
foreign import stopEgestFFI :: EgestKey -> Effect Unit
foreign import getStatsFFI :: EgestKey -> Effect (WebRTCStreamServerStats EgestKey)
foreign import setSlotConfigurationFFI :: EgestKey -> WorkflowHandle -> SlotConfiguration -> Effect Unit
foreign import getSlotConfigurationFFI :: EgestKey -> Effect (Maybe SlotConfiguration)

foreign import data WorkflowHandle :: Type

type CreateEgestPayload
  = { slotId :: SlotId
    , slotRole :: SlotRole
    , aggregatorPoP :: PoPName
    , slotCharacteristics :: SlotCharacteristics
    }

type RegistrationResp = (Either FailureReason (List ProfileName))

type ParentCallbacks
  = { startLocalOrRemoteStreamRelay :: LoadConfig -> OnBehalfOf -> CreateRelayPayload -> Effect (ResourceResp Server)
    , startLocalOrRemoteEgest :: LoadConfig -> OnBehalfOf -> CreateEgestPayload -> Effect (ResourceResp Server)
    }

type StateServerName = CachedInstanceState.StateServerName CachedState

type WebSocket = WsGun.WebSocket EgestUpstreamWsMessage DownstreamWsMessage

type CachedState = WebSocket

type State
  = { egestKey :: EgestKey
    , aggregatorPoP :: PoPName
    , parentCallbacks :: ParentCallbacks
    , slotCharacteristics :: SlotCharacteristics
    , loadConfig :: LoadConfig
    , thisServer :: Server
    , clientCount :: Int
    , clientStats :: Map String EgestSessionStats
    , lingerTime :: Milliseconds
    , relayCreationRetry :: Milliseconds
    , numForceDrainPhases :: Int
    , forceDrainTimeout :: Milliseconds
    , forceDrainPhaseTimeout :: Milliseconds
    , intraPoPLatency :: Milliseconds
    , aggregatorExitLingerTime :: Milliseconds
    , stopRef :: Maybe Ref
    , receivePortNumber :: Int
    , rtmpReceivePortNumber :: Int
    , lastEgestAuditTime :: Milliseconds
    , stateServerName :: StateServerName
    , relayWebSocket :: Maybe WebSocket
    , slotConfiguration :: Maybe SlotConfiguration
    , lastOnFI :: Int
    , dataObject :: Maybe DO.Object
    , activeProfiles :: List ProfileName
    , forceDrain :: Boolean
    , aggregatorExitTimerRef :: Maybe Ref

    , rtmpWorkflowHandle :: WorkflowHandle
    }



emptySessionStats :: String -> EgestSessionStats
emptySessionStats sessionId
  = { sessionId
    , audioPacketsSent: 0
    , audioOctetsSent: 0
    , videoPacketsSent: 0
    , videoOctetsSent: 0
    }

payloadToEgestKey :: CreateEgestPayload -> EgestKey
payloadToEgestKey payload = EgestKey payload.slotId payload.slotRole

data EgestBusMsg = EgestOnFI NativeJson Int
                 | EgestCurrentActiveProfiles (List ProfileName)
                 | EgestDataObjectMessage DO.Message
                 | EgestDataObjectUpdateResponse DO.ObjectUpdateResponseMessage
                 | EgestDataObjectBroadcast DO.Object
                 | EgestDrain Int Int (List String)

bus :: EgestKey -> Bus.Bus (Tuple2 Atom EgestKey) EgestBusMsg
bus egestKey = Bus.bus (tuple2 (atom "egestBus") egestKey)

data Msg = WriteEqLog
         | HandlerDown String
         | InitStreamRelays
         | MaybeStop Ref
         | IntraPoPBus IntraPoP.IntraPoPBusMessage
         | Gun WsGun.GunMsg
         | ForceDrainPhase Int
         | ForceDrainTimeout
         | AggregatorExitTimer Ref

serverName :: EgestKey -> ServerName State Msg
serverName egestKey = Names.egestInstanceName egestKey

isActive :: EgestKey -> Effect Boolean
isActive egestKey = Pinto.isRegistered (serverName egestKey)

startLink :: ParentCallbacks -> CreateEgestPayload -> StateServerName -> Effect StartLinkResult
startLink parentCallbacks payload stateServerName =
  Gen.startLink (serverName $ payloadToEgestKey payload) (init parentCallbacks payload stateServerName) handleInfo

stopAction :: EgestKey -> Maybe CachedState -> Effect Unit
stopAction egestKey mCachedState = do
  logStop "Egest stopping" {egestKey}
  IntraPoP.announceLocalEgestStopped egestKey
  fromMaybe (pure unit) $ WsGun.closeWebSocket <$> mCachedState
  stopEgestFFI egestKey
  pure unit

pendingClient :: EgestKey -> Effect RegistrationResp
pendingClient egestKey  =
  Gen.doCall ourServerName \state@{slotCharacteristics, loadConfig} -> do
    idleServerResp <- IntraPoP.getThisIdleServer $ Load.hasCapacityForEgestClient slotCharacteristics loadConfig
    case idleServerResp of
      Left _ ->
        pure $ CallReply (Left NoResource) state
      Right _ -> do
        state2 <- maybeResetStopTimer state
        pure $ CallReply (Right state.activeProfiles) state2

  where
    ourServerName = serverName egestKey
    maybeResetStopTimer state@{clientCount: 0, lingerTime} = do
      ref <- makeRef
      void $ Timer.sendAfter ourServerName (round $ Long.toNumber $ unwrap lingerTime) (MaybeStop ref)
      pure $ state{ stopRef = Just ref
                  }
    maybeResetStopTimer state =
      pure state

addClient :: Pid -> EgestKey -> String -> Effect RegistrationResp
addClient handlerPid egestKey sessionId =
  Gen.doCall ourServerName \state@{clientCount, clientStats, dataObject, activeProfiles, slotCharacteristics, loadConfig} -> do

    idleServerResp <- IntraPoP.getThisIdleServer $ Load.hasCapacityForEgestClient slotCharacteristics loadConfig
    case idleServerResp of
      Left _ ->
        pure $ CallReply (Left NoResource) state
      Right _ -> do
        logInfo "Add client" {newCount: clientCount + 1}
        Gen.monitorPid ourServerName handlerPid (\_ -> (HandlerDown sessionId))
        maybeSend dataObject
        (Process handlerPid) ! (EgestCurrentActiveProfiles activeProfiles)
        pure $ CallReply (Right state.activeProfiles) state{ clientCount = clientCount + 1
                                                           , clientStats = Map.insert sessionId (emptySessionStats sessionId) clientStats
                                                           , stopRef = Nothing
                                                           }
  where
    ourServerName = serverName egestKey
    maybeSend Nothing = pure unit
    maybeSend (Just dataObject) = (Process handlerPid) ! (EgestDataObjectBroadcast dataObject)

getSlotConfiguration :: EgestKey -> Effect (Maybe SlotConfiguration)
getSlotConfiguration egestKey =
  Gen.call (serverName egestKey) (\state@{slotConfiguration} -> CallReply slotConfiguration state)

dataObjectSendMessage :: EgestKey -> DO.Message -> Effect Unit
dataObjectSendMessage egestKey msg =
  Gen.doCall (serverName egestKey)
  (\state@{relayWebSocket: mRelayWebSocket} -> do
    case mRelayWebSocket of
      Just socket -> void $ WsGun.send socket (EdgeToRelayDataObjectMessage msg)
      Nothing -> pure unit
    pure $ CallReply unit state
  )

dataObjectUpdate :: EgestKey -> DO.ObjectUpdateMessage -> Effect Unit
dataObjectUpdate egestKey updateMsg =
  Gen.doCall (serverName egestKey)
  (\state@{relayWebSocket: mRelayWebSocket} -> do
    case mRelayWebSocket of
      Just socket -> void $ WsGun.send socket (EdgeToRelayDataObjectUpdateMessage updateMsg)
      Nothing -> pure unit
    pure $ CallReply unit state
  )

statsUpdate :: EgestKey -> EgestSessionStats -> Effect Unit
statsUpdate egestKey stats@{sessionId} = do
  Gen.doCall (serverName egestKey) \state@{clientStats} -> do
    pure $ CallReply unit state{clientStats = Map.insert sessionId stats clientStats}

forceDrain :: EgestKey -> Effect Unit
forceDrain egestKey =
  Gen.doCast (serverName egestKey) doForceDrain
  where
    doForceDrain state@{ egestKey: EgestKey slotId slotRole
                       , aggregatorPoP
                       , slotCharacteristics
                       , loadConfig
                       , forceDrainTimeout
                       , intraPoPLatency
                       , parentCallbacks: { startLocalOrRemoteEgest } } = do
      logInfo "Egest entering force-drain mode" {egestKey}

      void $ Timer.sendAfter (serverName egestKey) (round $ Long.toNumber $ unwrap forceDrainTimeout) ForceDrainTimeout
      void $ Timer.sendAfter (serverName egestKey) (round $ Long.toNumber $ unwrap intraPoPLatency) (ForceDrainPhase 0)
      void $ startLocalOrRemoteEgest loadConfig LocalAgent {slotId, slotRole, aggregatorPoP, slotCharacteristics}

      pure $ CastNoReply state{forceDrain = true}

currentStats :: EgestKey -> Effect (EgestStats List)
currentStats egestKey@(EgestKey slotId slotRole) =
  Gen.doCall (serverName egestKey) \state@{clientCount, clientStats} -> do
    now <- Erl.systemTimeMs
    let
      stats = { egestKey
              , timestamp: now
              , clientCount
              , sessions: Map.values clientStats
              }
    pure $ CallReply stats state

toEgestServer :: Server -> EgestServer
toEgestServer = unwrap >>> wrap

init :: ParentCallbacks -> CreateEgestPayload -> StateServerName -> Effect State
init parentCallbacks payload@{slotId, slotRole, aggregatorPoP, slotCharacteristics} stateServerName = do
  Logger.addLoggerContext $ PerSlot { slotId, slotRole, slotName: Nothing}
  { eqLogIntervalMs
  , lingerTimeMs
  , relayCreationRetryMs
  , reserveForPotentialNumClients
  , decayReserveMs
  , forceDrainTimeoutMs
  , numForceDrainPhases
  , aggregatorExitLingerTimeMs
  } <- Config.egestAgentConfig
  { intraPoPLatencyMs } <- Config.globalConfig
  loadConfig <- Config.loadConfig
  thisServer <- PoPDefinition.getThisServer

  let
    egestKey = payloadToEgestKey payload
    relayKey = RelayKey slotId slotRole
    LoadFixedCost { cpu: cpuPerClient
                  , network: networkPerClient} = Load.egestClientCost slotCharacteristics loadConfig thisServer
    predictedLoad = PredictedLoad { cost: LoadFixedCost { cpu: wrap $ (unwrap cpuPerClient) * (toNumber reserveForPotentialNumClients)
                                                        , network: wrap $ (unwrap networkPerClient) * reserveForPotentialNumClients
                                                        }
                                  , decayTime: Milliseconds $ Long.fromInt decayReserveMs
                                  }

  { mediaGateway } <- Config.featureFlags
  receivePortNumber /\ rtmpReceivePortNumber /\ rtmpWorkflowHandle /\ _  <- Tuple.toNested3 <$> startEgestReceiverFFI egestKey mediaGateway
  void $ Bus.subscribe (serverName egestKey) IntraPoP.bus IntraPoPBus
  logStart "Egest starting" {payload, receivePortNumber, rtmpReceivePortNumber}

  publicListenIp <- Env.publicListenIp

  

  now <- systemTimeMs
  IntraPoP.announceLocalEgestIsAvailable egestKey
  void $ Timer.sendAfter (serverName egestKey) 0 InitStreamRelays
  void $ Timer.sendEvery (serverName egestKey) eqLogIntervalMs WriteEqLog

  Load.addPredictedLoad (egestKeyToAgentKey egestKey) predictedLoad

  Gen.registerExternalMapping (serverName egestKey) (\m -> Gun <$> (WsGun.messageMapper m))
  Gen.registerTerminate (serverName egestKey) terminate

  let
    state = { egestKey
            , aggregatorPoP
            , parentCallbacks
            , slotCharacteristics
            , loadConfig
            , thisServer
            , clientCount : 0
            , clientStats : Map.empty
            , lingerTime : Milliseconds $ Long.fromInt lingerTimeMs
            , relayCreationRetry : Milliseconds $ Long.fromInt relayCreationRetryMs
            , forceDrainTimeout : Milliseconds $ Long.fromInt forceDrainTimeoutMs
            , forceDrainPhaseTimeout : Milliseconds $ Long.fromInt $ (forceDrainTimeoutMs - intraPoPLatencyMs) / (numForceDrainPhases + 1)
            , numForceDrainPhases
            , intraPoPLatency : Milliseconds $ Long.fromInt intraPoPLatencyMs
            , aggregatorExitLingerTime : Milliseconds $ Long.fromInt aggregatorExitLingerTimeMs
            , stopRef : Nothing
            , receivePortNumber
            , rtmpReceivePortNumber
            , lastEgestAuditTime: now
            , stateServerName
            , relayWebSocket: Nothing
            , slotConfiguration: Nothing
            , dataObject: Nothing
            , lastOnFI: 0
            , activeProfiles: nil
            , forceDrain: false
            , aggregatorExitTimerRef: Nothing
            , rtmpWorkflowHandle
            }
  pure state

handleInfo :: Msg -> State -> Effect (CastResult State)
handleInfo msg state@{egestKey: egestKey@(EgestKey slotId slotRole), thisServer} =
  case msg of
    WriteEqLog -> do
      Tuple endMs eqLines <- egestEqLines state
      traverse_ Audit.egestUpdate eqLines
      pure $ CastNoReply state{lastEgestAuditTime = endMs}

    HandlerDown sessionId -> do
      logInfo "client down!" {}
      state2 <- removeClient sessionId state
      pure $ CastNoReply state2

    InitStreamRelays -> CastNoReply <$> initStreamRelay state

    MaybeStop ref -> maybeStop ref

    IntraPoPBus (IngestAggregatorStarted _ _) ->
      pure $ CastNoReply state

    IntraPoPBus (IngestAggregatorExited (AggregatorKey exitedSlotId exitedSlotRole) serverAddress)
      | exitedSlotId == slotId && exitedSlotRole == slotRole -> do
        ref <- Erl.makeRef
        void $ Timer.sendAfter (serverName egestKey) (round $ Long.toNumber $ unwrap state.aggregatorExitLingerTime) (AggregatorExitTimer ref)
        pure $ CastNoReply $ state{aggregatorExitTimerRef = Just ref}
      | otherwise -> pure $ CastNoReply state

    IntraPoPBus (StreamRelayStarted _ _) ->
      pure $ CastNoReply state

    IntraPoPBus (StreamRelayExited _ _) ->
      pure $ CastNoReply state

    IntraPoPBus (EgestStarted startedEgestKey startedServer) | startedEgestKey == egestKey
                                                               && startedServer /= thisServer ->
      -- A twin just started on another server
      if state.forceDrain then do
        logInfo "ForceDrain mode: Twin just started; announcing that we have stopped" {egestKey}
        announceLocalEgestStopped egestKey
        pure $ CastNoReply state
      else do
        -- Multiple egest instances is normal
        pure $ CastNoReply state

    IntraPoPBus (EgestStarted _ _) ->
      pure $ CastNoReply state

    IntraPoPBus (EgestExited exitedKey exitedServer) | exitedKey == egestKey
                                                       && exitedServer == thisServer ->
      -- We just exited!  Only makes sense in forceDrain mode where we announce stop prior to stopping
      if state.forceDrain then do
        logInfo "ForceDrain mode: Announcement of our exit" {egestKey}
        pure $ CastNoReply state
      else do
        logInfo "Normal mode: Unexpected announcement of our exit" {egestKey}
        pure $ CastNoReply state

    IntraPoPBus (EgestExited _ _) ->
      pure $ CastNoReply state

    IntraPoPBus (VmReset _ _ _) ->
      pure $ CastNoReply state

    ForceDrainPhase n  -> do
        logInfo "Egest draining clients" {phase: n}
        drainClients state n

    ForceDrainTimeout -> do
        logInfo "Egest stopping due to force drain timeout" {egestKey}
        pure $ CastStop state

    AggregatorExitTimer ref
      | Just ref == state.aggregatorExitTimerRef -> do
        mAggregator <- IntraPoP.whereIsIngestAggregator (egestKeyToAggregatorKey egestKey)
        case mAggregator of
          Nothing -> pure $ CastStop state
          Just _ -> pure $ CastNoReply state
      | otherwise ->
        pure $ CastNoReply state

    Gun gunMsg ->
      processGunMessage state gunMsg

  where
    maybeStop ref
      | (state.clientCount == 0) && (Just ref == state.stopRef) = doStop state
      | otherwise = pure $ CastNoReply state

terminate :: TerminateReason -> State -> Effect Unit
terminate Normal state = do
  logInfo "EgestInstance terminating" {reason: Normal}
  pure unit
terminate reason state = do
  logInfo "EggestInstance terminating" {reason}
  Tuple endMs eqLines <- egestEqLines state
  traverse_ Audit.egestStop eqLines
  pure unit

processGunMessage :: State -> WsGun.GunMsg -> Effect (CastResult State)
processGunMessage state@{relayWebSocket: Nothing} gunMsg =
  pure $ CastNoReply state

processGunMessage state@{relayWebSocket: Just socket, egestKey, lastOnFI, rtmpWorkflowHandle} gunMsg =
  if WsGun.isSocketForMessage gunMsg socket then do
    processResponse <- WsGun.processMessage socket gunMsg
    case processResponse of
      Left error -> do
        logInfo "Gun process error" {error}
        pure $ CastNoReply state

      Right (WsGun.Internal _) ->
        pure $ CastNoReply state

      Right (WsGun.WebSocketUpdate newSocket) ->
        pure $ CastNoReply state{relayWebSocket = Just newSocket}

      Right WsGun.WebSocketUp -> do
        logInfo "Relay WebSocket up" {}
        pure $ CastNoReply state

      Right WsGun.WebSocketDown -> do
        logInfo "Relay WebSocket down" {}
        CastNoReply <$> initStreamRelay state

      Right (WsGun.Frame (SlotConfig slotConfiguration))
        | Nothing <- state.slotConfiguration -> do
          logInfo "Received slot configuration" {slotConfiguration}
          setSlotConfigurationFFI egestKey rtmpWorkflowHandle slotConfiguration
          pure $ CastNoReply state{slotConfiguration = Just slotConfiguration}

        | otherwise ->
          pure $ CastNoReply state

      Right (WsGun.Frame (OnFI {payload, pts})) | pts > lastOnFI -> do
        Bus.raise (bus egestKey) (EgestOnFI payload pts)
        pure $ CastNoReply state{lastOnFI = pts}

      Right (WsGun.Frame (OnFI {payload, pts})) ->
        pure $ CastNoReply state

      Right (WsGun.Frame (CurrentActiveProfiles activeProfiles@(ActiveProfiles {profiles}))) -> do
        shouldProcess <- DataObject.shouldProcessMessage egestKey activeProfiles
        if shouldProcess then do
          Bus.raise (bus egestKey) (EgestCurrentActiveProfiles profiles)
          pure $ CastNoReply state{activeProfiles = profiles}
        else
          pure $ CastNoReply state

      Right (WsGun.Frame (DataObjectMessage dataObjectMsg)) -> do
        shouldProcess <- DataObject.shouldProcessMessage egestKey dataObjectMsg
        if shouldProcess then Bus.raise (bus egestKey) (EgestDataObjectMessage dataObjectMsg)
        else pure unit
        pure $ CastNoReply state

      Right (WsGun.Frame (DataObjectUpdateResponse dataObjectMsg)) -> do
        shouldProcess <- DataObject.shouldProcessMessage egestKey dataObjectMsg
        if shouldProcess then Bus.raise (bus egestKey) (EgestDataObjectUpdateResponse dataObjectMsg)
        else pure unit
        pure $ CastNoReply state

      Right (WsGun.Frame (DataObject dataObjectMsg@(ObjectBroadcastMessage {object: dataObject}))) -> do
        shouldProcess <- DataObject.shouldProcessMessage egestKey dataObjectMsg
        if shouldProcess then do
          Bus.raise (bus egestKey) (EgestDataObjectBroadcast dataObject)
          pure $ CastNoReply state{dataObject = Just dataObject}
        else
          pure $ CastNoReply state

  else
    pure $ CastNoReply state

drainClients :: State -> Int -> Effect (CastResult State)
drainClients state@{clientCount: 0} _phase =
  pure $ CastStop state

drainClients state@{numForceDrainPhases} phase | phase > numForceDrainPhases =
  pure $ CastStop state

drainClients state@{egestKey, forceDrainPhaseTimeout, numForceDrainPhases, thisServer} phase = do
  egests <- (map (unwrap <<< extractAddress)) <$> List.filter ((/=) thisServer) <$> IntraPoP.whereIsEgest egestKey
  logInfo "Draining clients" { phase
                             , numPhases: numForceDrainPhases
                             , alternates: egests}
  let
    phase' = if phase == numForceDrainPhases then 0
             else phase
    numForceDrainPhases' = if phase == numForceDrainPhases then 1
                           else numForceDrainPhases
  Bus.raise (bus egestKey) (EgestDrain phase' numForceDrainPhases' egests)
  void $ Timer.sendAfter (serverName egestKey) (round $ Long.toNumber $ unwrap forceDrainPhaseTimeout) (ForceDrainPhase (phase + 1))
  pure $ CastNoReply state

removeClient :: String -> State -> Effect State
removeClient sessionId state@{clientCount: 0} = do
  logInfo "Remove client - already zero" {}
  pure $ state

removeClient sessionId state@{clientCount: 1, lingerTime, egestKey} = do
  ref <- makeRef
  logInfo "Last client gone, start stop timer" {}
  void $ Timer.sendAfter (serverName egestKey) (round $ Long.toNumber $ unwrap lingerTime) (MaybeStop ref)
  pure $ state{ clientCount = 0
              , clientStats = (Map.empty :: Map String EgestSessionStats)
              , stopRef = Just ref
              }

removeClient sessionId state@{clientCount, clientStats} = do
  logInfo "Remove client" { newCount: clientCount - 1 }
  pure $ state{ clientCount = clientCount - 1
              , clientStats = Map.delete sessionId clientStats}

egestEqLines :: State -> Effect (Tuple Milliseconds (List Audit.EgestEqLine))
egestEqLines state@{ egestKey: egestKey@(EgestKey slotId _slotRole)
                   , thisServer: (Server {address: thisServerAddr})
                   , clientStats
                   , lastEgestAuditTime: startMs
                   , slotConfiguration} = do
  endMs <- systemTimeMs
  {sessionInfo} <- getStatsFFI egestKey
  pure $ Tuple endMs ((egestEqLine slotId slotConfiguration (unwrap thisServerAddr) startMs endMs clientStats) <$> toUnfoldable sessionInfo)

egestEqLine :: SlotId -> Maybe SlotConfiguration -> String -> Milliseconds -> Milliseconds -> (Map String EgestSessionStats) -> Tuple String WebRTCSessionManagerStats -> Audit.EgestEqLine
egestEqLine slotId slotConfiguration thisServerAddr startMs endMs clientStatsMap (Tuple sessionId {channels}) =
  let
    clientStats = fromMaybe (emptySessionStats sessionId) $ lookup sessionId clientStatsMap
    receiverAccumulate acc {lostTotal} = acc + lostTotal

    channelInitial = {writtenAcc: 0, readAcc: 0, lostAcc: 0, remoteAddress: ""}

    channelAccumulate {writtenAcc, readAcc, lostAcc} {octetsSent, octetsReceived, remoteAddress, receiverInfo} =
      { writtenAcc: writtenAcc + octetsSent
      , readAcc: readAcc + octetsReceived
      , lostAcc: foldl receiverAccumulate lostAcc (values receiverInfo)
      , remoteAddress
      }

    {writtenAcc, readAcc, lostAcc, remoteAddress} = foldl channelAccumulate channelInitial (values channels)

    shortName = fromMaybe (wrap "n/a") (_.rtmpShortName <$> slotConfiguration)
  in

  { egestIp: thisServerAddr
  , egestPort: -1
  , subscriberIp: remoteAddress
  , username: sessionId
  , rtmpShortName: shortName
  , slotId
  , connectionType: WebRTC
  , startMs
  , endMs
  , bytesWritten: clientStats.audioOctetsSent + clientStats.videoOctetsSent
  , bytesRead: readAcc
  , lostPackets: lostAcc
  }

doStop :: State -> Effect (CastResult State)
doStop state@{egestKey} = do
  -- Stop actions are all performed in stopAction, which gets called by the CachedState gen_server
  pure $ CastStop state

initStreamRelay :: State -> Effect State
initStreamRelay state@{relayCreationRetry, egestKey: egestKey@(EgestKey slotId slotRole), aggregatorPoP, thisServer, stateServerName} = do
  relayResp <- findOrStartRelayForStream state
  logInfo "From findOrStartRelayForStream" {relayResp}
  case relayResp of
    Left _ ->
      do
        void $ Timer.sendAfter (serverName egestKey) (round $ Long.toNumber $ unwrap relayCreationRetry) InitStreamRelays
        pure state

    Right (Local local) ->
      tryConfigureAndRegister local

    Right (Remote remote) ->
      tryConfigureAndRegister remote

  where
    tryConfigureAndRegister relayServer = do
      wsUrl <-  System.makeWsUrl relayServer $ System.RelayRegisteredEgestWs slotId slotRole (extractAddress thisServer) state.receivePortNumber state.rtmpReceivePortNumber
      maybeWebSocket <- hush <$> WsGun.openWebSocket (serverName egestKey) Gun wsUrl
      case maybeWebSocket of
        Just webSocket -> do
          CachedInstanceState.recordInstanceData stateServerName webSocket
          pure state{ relayWebSocket = Just webSocket
                    }
        _ -> do
          void $ Timer.sendAfter (serverName egestKey) (round $ Long.toNumber $ unwrap relayCreationRetry) InitStreamRelays
          pure state

--------------------------------------------------------------------------------
-- Do we have a relay in this pop - yes -> use it
-- Does the stream have an origin (aggregator) - if so build a streamRelay chain to that origin
-- otherwise 404
--------------------------------------------------------------------------------
findOrStartRelayForStream :: State -> Effect (ResourceResp Server)
findOrStartRelayForStream state@{ egestKey: EgestKey slotId slotRole
                                , aggregatorPoP
                                , slotCharacteristics
                                , parentCallbacks: { startLocalOrRemoteStreamRelay }
                                , loadConfig} = do
  mRelay <- IntraPoP.whereIsStreamRelayWithLocalOrRemote $ RelayKey slotId slotRole
  maybe' (\_ -> startLocalOrRemoteStreamRelay loadConfig LocalAgent payload) (pure <<< Right) mRelay
  where
    payload = {slotId, slotRole, aggregatorPoP, slotCharacteristics}

toRelayServer :: forall a b. Newtype a b => Newtype RelayServer b => a -> RelayServer
toRelayServer = unwrap >>> wrap

 -- IngestAggregator - .... - .... - ... - Egest
 -- TheirPoP ............................. OurPoP

 -- 1. compute pop route ->
 --     1. [ ThePoP ]
 --     2.
 --        [ TheirPoP, IntermediatePoP, OurPoP ]
 --        [ TheirPoP, OtherIntermediatePoP1, OtherIntermediaPoP2, OurPoP ]

 -- 2. Create the relay for our pop - passing it the entirety of both chains
 --    detail: what does that means? is it just an HTTP request to some endpoint? yes

 -- that is everything


  -- from our relay's point of view, it needs to:
  -- if we're in the same pop as the aggregator - source from the ingest aggregator directly
  -- if we're in a different pop
  --   for each of the chains, pick the next pop in the next chain, and ask it to relay to us passing in the chain information relevant to it
  --      detail: to what server in the next pop do we talk?

  -- additional thoughts on load and stuff:
  -- If aggregator is in this pop pick server for the relay
  --   Needs capacity
  --   Prefer with most capacity (if any have enough) and create a relay and an edge on it
  -- If we are on the same server as the IngestAggregator and we have capacity, then create a single relay here
  -- same pop -> 1) If server with aggregator has cap


-- createRelayInThisPoP :: EgestKey -> PoPName -> List ViaPoPs -> Server -> Effect (Either FailureReason Server)
-- createRelayInThisPoP slotId thisPoPName routes ingestAggregator = do
--   maybeCandidateRelayServer <- IntraPoP.getIdleServer (const true)

--   case (spy "maybeCandidateRelayServer" maybeCandidateRelayServer) of
--     Just candidateRelayServer ->
--       let
--         url = makeUrl candidateRelayServer RelayE

--         request =
--           { slotId,
--             aggregator: ingestAggregator
--           , routes: List.toUnfoldable <$> List.toUnfoldable routes
--           } :: CreateRelayPayload
--       in
--       do
--         -- TODO / thoughts - do we wait for the entire relay chain to exist before returning?
--         -- what if there isn't enough resource on an intermediate PoP?
--         -- Single relay that goes direct?
--         restResult <- SpudGun.postJson url request
--         pure $ Right $ serverLoadToServer candidateRelayServer

--     Nothing ->
--       pure $ Left NoResource


--------------------------------------------------------------------------------
-- Log helpers
--------------------------------------------------------------------------------
domain :: List Atom
domain = Agent.Egest # show # atom # singleton

logInfo :: forall report. String -> { | report } -> Effect Unit
logInfo = Logger.info <<< Logger.traceMetadata domain

logStart :: forall report. String -> { | report } -> Effect Unit
logStart = Logger.info <<< Logger.eventMetadata domain Logger.Start

logStop :: forall report. String -> { | report } -> Effect Unit
logStop = Logger.info <<< Logger.eventMetadata domain Logger.Stop
