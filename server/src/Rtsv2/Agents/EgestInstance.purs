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
  , CreateEgestPayload

  , CachedState
  , WebSocket
  , StateServerName
  , domain
) where

import Prelude

import Bus as Bus
import Data.Either (Either(..), hush)
import Data.Foldable (foldl)
import Data.Int (round, toNumber)
import Data.Maybe (Maybe(..), fromMaybe, maybe')
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Erl.Atom (Atom, atom)
import Erl.Data.List (List, nil, singleton)
import Erl.Data.Map (values)
import Erl.Data.Tuple (Tuple2, tuple2)
import Erl.Process (Process(..), (!))
import Erl.Process.Raw (Pid)
import Erl.Utils (Ref, makeRef, systemTimeMs)
import Logger (Logger)
import Logger as Logger
import Pinto (ServerName, StartLinkResult)
import Pinto as Pinto
import Pinto.Gen (CallResult(..), CastResult(..))
import Pinto.Gen as Gen
import Pinto.Timer as Timer
import Rtsv2.Agents.CachedInstanceState as CachedInstanceState
import Rtsv2.Agents.EgestInstance.WebRTCTypes (WebRTCStreamServerStats, WebRTCSessionManagerStats)
import Rtsv2.Agents.IntraPoP (IntraPoPBusMessage(..))
import Rtsv2.Agents.IntraPoP as IntraPoP
import Rtsv2.Agents.SlotTypes (SlotConfiguration)
import Rtsv2.Agents.StreamRelaySup as StreamRelaySup
import Rtsv2.Agents.StreamRelayTypes (ActiveProfiles(..), DownstreamWsMessage(..), EgestUpstreamWsMessage(..))
import Rtsv2.Audit as Audit
import Rtsv2.Config (LoadConfig)
import Rtsv2.Config as Config
import Rtsv2.DataObject (ObjectBroadcastMessage(..))
import Rtsv2.DataObject as DO
import Rtsv2.DataObject as DataObject
import Rtsv2.Load as Load
import Rtsv2.Names as Names
import Rtsv2.PoPDefinition as PoPDefinition
import Shared.Common (Milliseconds)
import Shared.Rtsv2.Agent (SlotCharacteristics)
import Shared.Rtsv2.Agent as Agent
import Shared.Rtsv2.Agent.State as PublicState
import Shared.Rtsv2.JsonLd as JsonLd
import Shared.Rtsv2.LlnwApiTypes (StreamIngestProtocol(..))
import Shared.Rtsv2.Router.Endpoint (Endpoint(..), makeWsUrl)
import Shared.Rtsv2.Stream (AggregatorKey(..), EgestKey(..), ProfileName, RelayKey(..), SlotId, SlotRole)
import Shared.Rtsv2.Types (EgestServer(..), FailureReason(..), LocalOrRemote(..), RegistrationResp, RelayServer, ResourceResp, Server, extractAddress)
import WsGun as WsGun

foreign import startEgestReceiverFFI :: EgestKey -> Boolean -> Effect Int
foreign import getStatsFFI :: EgestKey -> Effect (WebRTCStreamServerStats EgestKey)
foreign import setSlotConfigurationFFI :: EgestKey -> SlotConfiguration -> Effect Unit
foreign import getSlotConfigurationFFI :: EgestKey -> Effect (Maybe SlotConfiguration)

type CreateEgestPayload
  = { slotId :: SlotId
    , slotRole :: SlotRole
    , aggregator :: Server
    , slotCharacteristics :: SlotCharacteristics
    }

type StateServerName = CachedInstanceState.StateServerName CachedState

type WebSocket = WsGun.WebSocket EgestUpstreamWsMessage DownstreamWsMessage

type CachedState = WebSocket

type State
  = { egestKey :: EgestKey
    , aggregator :: Server
    , slotCharacteristics :: SlotCharacteristics
    , loadConfig :: LoadConfig
    , thisServer :: EgestServer
    , clientCount :: Int
    , lingerTime :: Milliseconds
    , relayCreationRetry :: Milliseconds
    , stopRef :: Maybe Ref
    , receivePortNumber :: Int
    , lastEgestAuditTime :: Milliseconds
    , stateServerName :: StateServerName
    , relayWebSocket :: Maybe WebSocket
    , slotConfiguration :: Maybe SlotConfiguration
    , lastOnFI :: Int
    , dataObject :: Maybe DO.Object
    , activeProfiles :: List ProfileName
    }

payloadToEgestKey :: CreateEgestPayload -> EgestKey
payloadToEgestKey payload = EgestKey payload.slotId payload.slotRole

data EgestBusMsg = EgestOnFI Int Int
                 | EgestCurrentActiveProfiles (List ProfileName)
                 | EgestDataObjectMessage DO.Message
                 | EgestDataObjectUpdateResponse DO.ObjectUpdateResponseMessage
                 | EgestDataObjectBroadcast DO.Object

bus :: EgestKey -> Bus.Bus (Tuple2 Atom EgestKey) EgestBusMsg
bus egestKey = Bus.bus (tuple2 (atom "egestBus") egestKey)

data Msg = WriteEqLog
         | HandlerDown
         | InitStreamRelays
         | MaybeStop Ref
         | IntraPoPBus IntraPoP.IntraPoPBusMessage
         | Gun WsGun.GunMsg

serverName :: EgestKey -> ServerName State Msg
serverName egestKey = Names.egestInstanceName egestKey

isActive :: EgestKey -> Effect Boolean
isActive egestKey = Pinto.isRegistered (serverName egestKey)

startLink :: CreateEgestPayload -> StateServerName -> Effect StartLinkResult
startLink payload stateServerName = Gen.startLink (serverName $ payloadToEgestKey payload) (init payload stateServerName) handleInfo

stopAction :: EgestKey -> Maybe CachedState -> Effect Unit
stopAction egestKey mCachedState = do
  logStop "Egest stopping" {egestKey}
  IntraPoP.announceLocalEgestStopped egestKey
  fromMaybe (pure unit) $ WsGun.closeWebSocket <$> mCachedState
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
        pure $ CallReply (Right unit) state2

  where
    ourServerName = serverName egestKey
    maybeResetStopTimer state@{clientCount: 0, lingerTime} = do
      ref <- makeRef
      _ <- Timer.sendAfter ourServerName (round $ unwrap lingerTime) (MaybeStop ref)
      pure $ state{ stopRef = Just ref
                  }
    maybeResetStopTimer state =
      pure state

addClient :: Pid -> EgestKey -> Effect RegistrationResp
addClient handlerPid egestKey =
  Gen.doCall ourServerName \state@{clientCount, dataObject, activeProfiles, slotCharacteristics, loadConfig} -> do

    idleServerResp <- IntraPoP.getThisIdleServer $ Load.hasCapacityForEgestClient slotCharacteristics loadConfig
    case idleServerResp of
      Left _ ->
        pure $ CallReply (Left NoResource) state
      Right _ -> do
        logInfo "Add client" {newCount: clientCount + 1}
        Gen.monitorPid ourServerName handlerPid (\_ -> HandlerDown)
        maybeSend dataObject
        (Process handlerPid) ! (EgestCurrentActiveProfiles activeProfiles)
        pure $ CallReply (Right unit) state{ clientCount = clientCount + 1
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
    _ <- case mRelayWebSocket of
           Just socket -> void $ WsGun.send socket (EdgeToRelayDataObjectMessage msg)
           Nothing -> pure unit
    pure $ CallReply unit state
  )

dataObjectUpdate :: EgestKey -> DO.ObjectUpdateMessage -> Effect Unit
dataObjectUpdate egestKey updateMsg =
  Gen.doCall (serverName egestKey)
  (\state@{relayWebSocket: mRelayWebSocket} -> do
    _ <- case mRelayWebSocket of
           Just socket -> void $ WsGun.send socket (EdgeToRelayDataObjectUpdateMessage updateMsg)
           Nothing -> pure unit
    pure $ CallReply unit state
  )

currentStats :: EgestKey -> Effect PublicState.Egest
currentStats egestKey@(EgestKey slotId slotRole) =
  Gen.call (serverName egestKey) \state@{thisServer, clientCount} ->
    CallReply (JsonLd.egestStatsNode slotId slotRole (wrap (unwrap thisServer)) {clientCount}) state

toEgestServer :: Server -> EgestServer
toEgestServer = unwrap >>> wrap

init :: CreateEgestPayload -> StateServerName -> Effect State
init payload@{slotId, slotRole, aggregator, slotCharacteristics} stateServerName = do
  let
    egestKey = payloadToEgestKey payload
    relayKey = RelayKey slotId slotRole
  { useMediaGateway } <- Config.featureFlags
  receivePortNumber <- startEgestReceiverFFI egestKey useMediaGateway
  _ <- Bus.subscribe (serverName egestKey) IntraPoP.bus IntraPoPBus
  logStart "Egest starting" {payload, receivePortNumber}
  {eqLogIntervalMs, lingerTimeMs, relayCreationRetryMs} <- Config.egestAgentConfig
  now <- systemTimeMs
  thisServer <- PoPDefinition.getThisServer
  _ <- IntraPoP.announceLocalEgestIsAvailable egestKey
  _ <- Timer.sendAfter (serverName egestKey) 0 InitStreamRelays
  _ <- Timer.sendEvery (serverName egestKey) eqLogIntervalMs WriteEqLog
  loadConfig <- Config.loadConfig

  Gen.registerExternalMapping (serverName egestKey) (\m -> Gun <$> (WsGun.messageMapper m))
  let
    state = { egestKey
            , aggregator
            , slotCharacteristics
            , loadConfig
            , thisServer : toEgestServer thisServer
            , clientCount : 0
            , lingerTime : wrap $ toNumber lingerTimeMs
            , relayCreationRetry : wrap $ toNumber relayCreationRetryMs
            , stopRef : Nothing
            , receivePortNumber
            , lastEgestAuditTime: now
            , stateServerName
            , relayWebSocket: Nothing
            , slotConfiguration: Nothing
            , dataObject: Nothing
            , lastOnFI: 0
            , activeProfiles: nil
            }
  maybeRelay <- IntraPoP.whereIsStreamRelay relayKey
  case maybeRelay of
    Just relay ->
      pure state
    Nothing -> do
      -- Optimistically attempt to launch a local relay - if it fails, our regular timer will sort it out
      _ <- StreamRelaySup.startLocalStreamRelay loadConfig {slotId, slotRole, aggregator, slotCharacteristics}
      pure state

handleInfo :: Msg -> State -> Effect (CastResult State)
handleInfo msg state@{egestKey: egestKey@(EgestKey slotId slotRole)} =
  case msg of
    WriteEqLog -> do
      Tuple endMs eqLines <- egestEqLines state
      _ <- traverse Audit.egestUpdate eqLines
      pure $ CastNoReply state{lastEgestAuditTime = endMs}

    HandlerDown -> do
      _ <- logInfo "client down!" {}
      state2 <- removeClient state
      pure $ CastNoReply state2

    InitStreamRelays -> CastNoReply <$> initStreamRelay state

    MaybeStop ref -> maybeStop ref

    IntraPoPBus (IngestAggregatorStarted _ _) ->
      pure $ CastNoReply state

    IntraPoPBus (IngestAggregatorExited (AggregatorKey exitedSlotId exitedSlotRole) serverAddress)
      | exitedSlotId == slotId && exitedSlotRole == slotRole -> doStop state
      | otherwise -> pure $ CastNoReply state

    IntraPoPBus (VmReset _ _ _) ->
      pure $ CastNoReply state

    Gun gunMsg ->
      processGunMessage state gunMsg

  where
    maybeStop ref
      | (state.clientCount == 0) && (Just ref == state.stopRef) = doStop state
      | otherwise = pure $ CastNoReply state

processGunMessage :: State -> WsGun.GunMsg -> Effect (CastResult State)
processGunMessage state@{relayWebSocket: Nothing} gunMsg =
  pure $ CastNoReply state

processGunMessage state@{relayWebSocket: Just socket, egestKey, lastOnFI} gunMsg =
  if WsGun.isSocketForMessage gunMsg socket then do
    processResponse <- WsGun.processMessage socket gunMsg
    case processResponse of
      Left error -> do
        _ <- logInfo "Gun process error" {error}
        pure $ CastNoReply state

      Right (WsGun.Internal _) ->
        pure $ CastNoReply state

      Right WsGun.WebSocketUp -> do
        _ <- logInfo "Relay WebSocket up" {}
        pure $ CastNoReply state

      Right WsGun.WebSocketDown -> do
        _ <- logInfo "Relay WebSocket down" {}
        CastNoReply <$> initStreamRelay state

      Right (WsGun.Frame (SlotConfig slotConfiguration))
        | Nothing <- state.slotConfiguration -> do
          _ <- logInfo "Received slot configuration" {slotConfiguration}
          setSlotConfigurationFFI egestKey slotConfiguration
          pure $ CastNoReply state{slotConfiguration = Just slotConfiguration}

        | otherwise ->
          pure $ CastNoReply state

      Right (WsGun.Frame (OnFI {timestamp, pts})) | timestamp > lastOnFI -> do
        Bus.raise (bus egestKey) (EgestOnFI timestamp pts)
        pure $ CastNoReply state{lastOnFI = timestamp}

      Right (WsGun.Frame (OnFI {timestamp, pts})) ->
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
          _ <- Bus.raise (bus egestKey) (EgestDataObjectBroadcast dataObject)
          pure $ CastNoReply state{dataObject = Just dataObject}
        else
          pure $ CastNoReply state

  else
    pure $ CastNoReply state

removeClient :: State -> Effect State
removeClient state@{clientCount: 0} = do
  logInfo "Remove client - already zero" {}
  pure $ state

removeClient state@{clientCount: 1, lingerTime, egestKey} = do
  ref <- makeRef
  logInfo "Last client gone, start stop timer" {}
  _ <- Timer.sendAfter (serverName egestKey) (round $ unwrap lingerTime) (MaybeStop ref)
  pure $ state{ clientCount = 0
              , stopRef = Just ref
              }

removeClient state@{clientCount} = do
  logInfo "Remove client" { newCount: clientCount - 1 }
  pure $ state{ clientCount = clientCount - 1 }

egestEqLines :: State -> Effect (Tuple Milliseconds (List Audit.EgestEqLine))
egestEqLines state@{ egestKey: egestKey@(EgestKey slotId _slotRole)
                   , thisServer: (Egest {address: thisServerAddr})
                   , lastEgestAuditTime: startMs} = do
  endMs <- systemTimeMs
  {sessionInfo} <- getStatsFFI egestKey
  pure $ Tuple endMs ((egestEqLine slotId (unwrap thisServerAddr) startMs endMs) <$> values sessionInfo)

egestEqLine :: SlotId -> String -> Milliseconds -> Milliseconds -> WebRTCSessionManagerStats -> Audit.EgestEqLine
egestEqLine slotId thisServerAddr startMs endMs {channels} =
  let
    receiverAccumulate acc {lostTotal} = acc + lostTotal

    channelInitial = {writtenAcc: 0, readAcc: 0, lostAcc: 0, remoteAddress: ""}

    channelAccumulate {writtenAcc, readAcc, lostAcc} {octetsSent, octetsReceived, remoteAddress, receiverInfo} =
      { writtenAcc: writtenAcc + octetsSent
      , readAcc: readAcc + octetsReceived
      , lostAcc: foldl receiverAccumulate lostAcc (values receiverInfo)
      , remoteAddress
      }

    {writtenAcc, readAcc, lostAcc, remoteAddress} = foldl channelAccumulate channelInitial (values channels)
  in

  { egestIp: thisServerAddr
  , egestPort: -1
  , subscriberIp: remoteAddress
  , username: "n/a" -- TODO
  , slotId
  , connectionType: WebRTC
  , startMs
  , endMs
  , bytesWritten: writtenAcc
  , bytesRead: readAcc
  , lostPackets: lostAcc
  }

doStop :: State -> Effect (CastResult State)
doStop state@{egestKey} = do
  -- Stop actions are all performed in stopAction, which gets called by the CachedState gen_server
  pure $ CastStop state

initStreamRelay :: State -> Effect State
initStreamRelay state@{relayCreationRetry, egestKey: egestKey@(EgestKey slotId slotRole), aggregator, thisServer, stateServerName, slotConfiguration: mSlotConfig} = do
  case mSlotConfig of
    Nothing -> do
      relayResp <- findOrStartRelayForStream state

      case relayResp of
        Left _ ->
          do
            _ <- Timer.sendAfter (serverName egestKey) (round $ unwrap relayCreationRetry) InitStreamRelays
            pure state

        Right (Local local) ->
          tryConfigureAndRegister local

        Right (Remote remote) ->
          tryConfigureAndRegister remote
    Just _ ->
      pure state

  where
    tryConfigureAndRegister relayServer =
      do
        let
          wsUrl = makeWsUrl relayServer $ RelayRegisteredEgestWs slotId slotRole (extractAddress thisServer) state.receivePortNumber
        maybeWebSocket <- hush <$> WsGun.openWebSocket wsUrl
        case maybeWebSocket of
          Just webSocket -> do
            CachedInstanceState.recordInstanceData stateServerName webSocket
            pure state{ relayWebSocket = Just webSocket}
          Nothing -> do
            _ <- Timer.sendAfter (serverName egestKey) (round $ unwrap relayCreationRetry) InitStreamRelays
            pure state

--------------------------------------------------------------------------------
-- Do we have a relay in this pop - yes -> use it
-- Does the stream have an origin (aggregator) - if so build a streamRelay chain to that origin
-- otherwise 404
--------------------------------------------------------------------------------
findOrStartRelayForStream :: State -> Effect (ResourceResp Server)
findOrStartRelayForStream state@{ egestKey: EgestKey slotId slotRole
                                , thisServer
                                , aggregator
                                , slotCharacteristics
                                , loadConfig} = do
  mRelay <- IntraPoP.whereIsStreamRelayWithLocalOrRemote $ RelayKey slotId slotRole
  maybe' (\_ -> StreamRelaySup.startLocalOrRemoteStreamRelay loadConfig payload) (pure <<< Right) mRelay
  where
    payload = {slotId, slotRole, aggregator, slotCharacteristics}


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
--         _restResult <- SpudGun.postJson url request
--         pure $ Right $ serverLoadToServer candidateRelayServer

--     Nothing ->
--       pure $ Left NoResource


--------------------------------------------------------------------------------
-- Log helpers
--------------------------------------------------------------------------------
domain :: List Atom
domain = Agent.Egest # show # atom # singleton

logInfo :: forall a. Logger (Record a)
logInfo = Logger.doLog domain Logger.info

logStart :: forall a. Logger (Record a)
logStart = Logger.doLogEvent domain Logger.Start Logger.info

logStop :: forall a. Logger (Record a)
logStop = Logger.doLogEvent domain Logger.Stop Logger.info
