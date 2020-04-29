--------------------------------------------------------------------------------
-- Mermaid description of the state transitions for the dataobject ownership.
-- (paste into this: https://mermaid-js.github.io/mermaid-live-editor/#/edit/)
--
-- graph TD
--  p[Primary] --> backupExists(Backup Exists)
-- 	backupExists --No--> createObject(Create Object)
-- 	backupExists --Yes--> sendSynchronise(Send Synchronise)
-- 	sendSynchronise --> awaitResponse(Await Response)
-- 	awaitResponse --Backup Exits--> createObject
-- 	awaitResponse --No Object--> createObject
-- 	awaitResponse --Synchronise--> mergeObjects(Merge Objects)
-- 	mergeObjects --> sendLatest(Send Latest)
-- 	createObject --> sendLatest
-- 	sendLatest --> primaryOwned(Primary Owned)
-- 	primaryOwned --Backup Starts--> sendSynchronise2(Send Synchronise)
-- 	sendSynchronise2 --Backup Exits--> primaryOwned
-- 	sendSynchronise2 --No Object-->sendLatest
-- 	sendSynchronise2 --Synchronise--> mergeObjects
--
-- 	b[Backup] --> primaryExists(Primary Exists)
--  primaryExists --No--> bCreateObject(Create Object)
-- 	primaryExists --Yes--> bAwaitConnection(Await Connection)
-- 	bAwaitConnection --Primary Exits--> bCreateObject
-- 	bAwaitConnection --SendSynchronise Received--> bSendNoObject(Send NoObject Response)
-- 	bSendNoObject --> bPeerOwned(Peer Owned)
-- 	bCreateObject --> backupOwned(Backup Owned)
-- 	backupOwned --SendSynchronise Received--> bSendSyncResponse(Send Synchronise Response)
-- 	bSendSyncResponse --> bAwaitResponse(Await Latest)
-- 	bAwaitResponse --Receive Latest--> bPeerOwned
-- 	bAwaitResponse --Primary Exits--> backupOwned
-- 	bPeerOwned --Primary Exits--> backupOwned
--------------------------------------------------------------------------------

module Rtsv2.Agents.IngestAggregatorInstance
  ( startLink
  , stopAction
  , isInstanceAvailable
  , registerPrimary
  , registerIngest
  , registerRelay
  , getState
  , domain
  , dataObjectSendMessage
  , dataObjectUpdate
  , processMessageFromPrimary
  , CachedState
  , RegisteredRelay
  , StateServerName
  , IngestProcess
  , RelayProcess
  , streamDetailsToAggregatorKey
  ) where

import Prelude

import Bus as Bus
import Data.Either (Either(..), either, hush)
import Data.FoldableWithIndex (foldWithIndexM, foldlWithIndex)
import Data.Lens (Lens', set, view)
import Data.Lens.At (at)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (unwrap)
import Data.Symbol (SProxy(..))
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Erl.Atom (Atom, atom)
import Erl.Data.List (List, nil, (:))
import Erl.Data.Map (Map)
import Erl.Data.Map as Map
import Erl.Data.Tuple (Tuple3, toNested3, tuple3)
import Erl.Process (Process(..), (!))
import Erl.Process.Raw (Pid)
import Erl.Utils (Ref, shutdown)
import Erl.Utils as Erl
import Foreign (Foreign)
import Logger (Logger)
import Logger as Logger
import Pinto (ServerName, StartLinkResult, isRegistered)
import Pinto.Gen (CallResult(..), CastResult(..))
import Pinto.Gen as Gen
import Pinto.Timer as Timer
import Rtsv2.Agents.CachedInstanceState as CachedInstanceState
import Rtsv2.Agents.IntraPoP (IntraPoPBusMessage(..), announceLocalAggregatorIsAvailable, announceLocalAggregatorStopped)
import Rtsv2.Agents.IntraPoP as IntraPoP
import Rtsv2.Agents.SlotTypes (SlotConfiguration)
import Rtsv2.Agents.StreamRelayTypes (ActiveProfiles(..), AggregatorBackupToPrimaryWsMessage(..), AggregatorPrimaryToBackupWsMessage(..), AggregatorToIngestWsMessage(..), DownstreamWsMessage(..), WebSocketHandlerMessage(..))
import Rtsv2.Config as Config
import Rtsv2.DataObject as DO
import Rtsv2.Names as Names
import Rtsv2.PoPDefinition as PoPDefinition
import Shared.Rtsv2.Agent as Agent
import Shared.Rtsv2.Agent.State as PublicState
import Shared.Rtsv2.JsonLd as JsonLd
import Shared.Rtsv2.LlnwApiTypes (SlotProfile(..), StreamDetails, slotDetailsToSlotCharacteristics)
import Shared.Rtsv2.Router.Endpoint (Endpoint(..), makeUrlAddr, makeWsUrl)
import Shared.Rtsv2.Stream (AggregatorKey(..), IngestKey(..), ProfileName, SlotId, SlotRole(..), ingestKeyToAggregatorKey)
import Shared.Rtsv2.Types (DeliverTo, RelayServer, Server, ServerAddress, extractAddress)
import Shared.UUID (UUID)
import WsGun as WsGun

foreign import data WorkflowHandle :: Type
foreign import startWorkflowImpl :: UUID -> SlotRole -> Array (Tuple3 IngestKey String String) -> Effect (Tuple3 WorkflowHandle (List Pid) SlotConfiguration)
foreign import stopWorkflowImpl :: WorkflowHandle -> Effect Unit
foreign import addLocalIngestImpl :: WorkflowHandle -> IngestKey -> Effect Unit
foreign import addRemoteIngestImpl :: WorkflowHandle -> IngestKey -> String -> Effect Unit
foreign import removeIngestImpl :: WorkflowHandle -> IngestKey -> Effect Unit
foreign import registerStreamRelayImpl :: WorkflowHandle -> String -> Int -> Effect Unit
foreign import deRegisterStreamRelayImpl :: WorkflowHandle -> String -> Int -> Effect Unit
foreign import workflowMessageMapperImpl :: Foreign -> Maybe WorkflowMsg

type PrimaryToBackupWebsocket = WsGun.WebSocket AggregatorPrimaryToBackupWsMessage AggregatorBackupToPrimaryWsMessage
type BackupToPrimaryProcess = Process (WebSocketHandlerMessage AggregatorBackupToPrimaryWsMessage)

type RelayProcess = Process (WebSocketHandlerMessage DownstreamWsMessage)
type IngestProcess = Process (WebSocketHandlerMessage AggregatorToIngestWsMessage)

type RegisteredRelay = { port :: Int
                       , handler :: RelayProcess
                       }

type CachedState = { localIngests :: Map ProfileName IngestProcess
                   , remoteIngests :: Map ProfileName { ingestAddress :: ServerAddress
                                                      , handler :: IngestProcess }
                   , relays :: Map RelayServer RegisteredRelay
                   }

type StateServerName = CachedInstanceState.StateServerName CachedState

type PrimaryDataObject = DO.Object

data BackupDataObject = OwnedByUs DO.Object
                      | OwnedByPrimary DO.Object
                      | Synchronising DO.Object

type PrimaryDOState
  = { dataObject :: Maybe DO.Object
    , connectionToBackup :: Maybe PrimaryToBackupWebsocket
    }

type BackupDOState
  = { dataObject :: Maybe BackupDataObject
    , connectionToPrimary :: Maybe BackupToPrimaryProcess
    }

type State
  = { config :: Config.IngestAggregatorAgentConfig
    , slotId :: SlotId
    , slotRole :: SlotRole
    , peerRole :: SlotRole
    , cachedState :: CachedState
    , thisServer :: Server
    , aggregatorKey :: AggregatorKey
    , streamDetails :: StreamDetails
    , workflowHandle :: WorkflowHandle
    , webRtcStreamServers :: List Pid
    , stateServerName :: StateServerName
    , slotConfiguration :: SlotConfiguration
    , dataObjectState :: Either PrimaryDOState BackupDOState
    , maybeStopRef :: Maybe Ref
    }

data WorkflowMsg = Noop
                 | RtmpOnFI Int Int

data Msg
  = IntraPoPBus IntraPoP.IntraPoPBusMessage
  | AttemptConnectionToBackup
  | Gun WsGun.GunMsg
  | MaybeStop Ref
  | RelayDown RelayServer
  | IngestDown ProfileName ServerAddress Boolean
  | PrimaryHandlerDown
  | Workflow WorkflowMsg

isInstanceAvailable :: AggregatorKey -> Effect Boolean
isInstanceAvailable aggregatorKey = do
  bool <- isRegistered (serverName aggregatorKey)
  pure bool

payloadToAggregatorKey :: forall r. { slotId :: SlotId, slotRole :: SlotRole | r } -> AggregatorKey
payloadToAggregatorKey payload = AggregatorKey payload.slotId payload.slotRole

serverName :: AggregatorKey -> ServerName State Msg
serverName = Names.ingestAggregatorInstanceName

serverNameFromIngestKey :: IngestKey -> ServerName State Msg
serverNameFromIngestKey = serverName <<< ingestKeyToAggregatorKey

registerPrimary :: SlotId -> SlotRole -> BackupToPrimaryProcess -> Effect Boolean
registerPrimary slotId slotRole handler@(Process handlerPid) =
  Gen.doCall thisServerName doRegisterPrimary
  where
    doRegisterPrimary state@{dataObjectState: Right dos} = do
      Gen.monitorPid thisServerName handlerPid (\_ -> PrimaryHandlerDown)
      pure $ CallReply true state{dataObjectState = Right dos{connectionToPrimary = Just handler}}

    doRegisterPrimary state =
      -- We are primary, who the heck called us!
      pure $ CallReply false state

    thisServerName = serverName (AggregatorKey slotId slotRole)

registerIngest :: SlotId -> SlotRole -> ProfileName -> ServerAddress -> IngestProcess -> Effect Boolean
registerIngest slotId slotRole profileName ingestAddress handler@(Process handlerPid) =
  Gen.doCall thisServerName
  (\state@{thisServer} -> do
      let
        isLocal = extractAddress thisServer == ingestAddress
      result <- if isLocal
                then doAddLocalIngest profileName handler state
                else doAddRemoteIngest profileName handler ingestAddress state
      case result of
        Left unit ->
          pure $ CallReply false state
        Right state2 -> do
          Gen.monitorPid thisServerName handlerPid (\_ -> IngestDown profileName ingestAddress isLocal)
          pure $ CallReply true state2
  )
  where
    thisServerName = serverName (AggregatorKey slotId slotRole)

registerRelay :: SlotId -> SlotRole -> DeliverTo RelayServer -> RelayProcess -> Effect SlotConfiguration
registerRelay slotId slotRole deliverTo handler =
  Gen.doCall (serverName $ key)
  (\state@{thisServer, slotConfiguration, dataObjectState} -> do
      case getDataObject dataObjectState of
        Just dataObject -> do
          ref <- Erl.makeRef
          handler ! (WsSend $ DataObject $ DO.ObjectBroadcastMessage { object: dataObject
                                                                     , ref})
        _ ->
          pure unit
      activeProfiles <- currentActiveProfiles state
      handler ! (WsSend $ CurrentActiveProfiles activeProfiles)
      CallReply slotConfiguration <$> doRegisterRelay deliverTo handler state
  )
  where
    key = AggregatorKey slotId slotRole
    getDataObject (Left {dataObject: Nothing}) = Nothing
    getDataObject (Left {dataObject: Just dataObject}) = Just dataObject
    getDataObject (Right {dataObject: Nothing}) = Nothing
    getDataObject (Right {dataObject: Just (OwnedByUs dataObject)}) = Just dataObject
    getDataObject (Right {dataObject: Just (OwnedByPrimary dataObject)}) = Just dataObject
    getDataObject (Right {dataObject: Just (Synchronising dataObject)}) = Nothing

processMessageFromPrimary :: AggregatorKey -> AggregatorPrimaryToBackupWsMessage -> Effect Unit
processMessageFromPrimary aggregatorKey msg =
  Gen.doCall (serverName aggregatorKey) doProcessMessage
  where
    doProcessMessage state@{dataObjectState: Right dos@{dataObject: mDataObject}} =
      case msg of
        P2B_Synchronise -> do
          sendToPrimary dos (maybe B2P_SynchroniseNoObject (B2P_SynchroniseObject <<< getDataObject) mDataObject)
          pure $ CallReply unit state

        P2B_Latest latestObject -> do
          sendBroadcast state latestObject
          pure $ CallReply unit state{dataObjectState = Right dos{dataObject = Just (OwnedByPrimary latestObject)}}

        P2B_Message message -> do
          sendDownstream state (DataObjectMessage message)
          pure $ CallReply unit state

        P2B_UpdateResponse responseMessage -> do
          sendDownstream state (DataObjectUpdateResponse responseMessage)
          pure $ CallReply unit state

    doProcessMessage state@{dataObjectState: Left _} =
      -- We are primary, who the heck called us!
     pure $ CallReply unit state

    getDataObject (OwnedByUs dataObject) = dataObject
    getDataObject (OwnedByPrimary dataObject) = dataObject
    getDataObject (Synchronising dataObject) = dataObject

checkProfileInactive :: ProfileName -> State -> Boolean
checkProfileInactive profileName {cachedState: {localIngests, remoteIngests}} =
  not (Map.member profileName localIngests) && not (Map.member profileName remoteIngests)

getState :: AggregatorKey -> Effect (PublicState.IngestAggregator List)
getState aggregatorKey@(AggregatorKey slotId slotRole) = Gen.call (serverName aggregatorKey) getState'
  where
    getState' state@{streamDetails, cachedState: {localIngests, remoteIngests, relays}, thisServer} =
      CallReply (JsonLd.ingestAggregatorStateNode slotId gatherState thisServer) state
      where
        gatherState = { role: slotRole
                      , streamDetails
                      , activeProfiles: (\(Tuple profileName ingestAddress) -> JsonLd.activeIngestLocationNode slotId slotRole profileName ingestAddress) <$> allProfiles
                      , downstreamRelays: foldlWithIndex (\server acc {port} -> (JsonLd.downstreamRelayLocationNode slotId slotRole {server, port}) : acc) nil relays
                      }
        localProfiles outerAcc = foldlWithIndex (\profileName acc _handler -> (Tuple profileName (extractAddress thisServer)) : acc) outerAcc localIngests
        remoteProfiles outerAcc = foldlWithIndex (\profileName acc {ingestAddress} -> (Tuple profileName ingestAddress) : acc) outerAcc remoteIngests
        allProfiles = remoteProfiles $ localProfiles nil


dataObjectSendMessage :: AggregatorKey -> DO.Message -> Effect Unit
dataObjectSendMessage aggregatorKey msg@(DO.Message {destination: DO.Publisher}) =
  Gen.doCall (serverName aggregatorKey)
  (\state@{cachedState: {localIngests, remoteIngests}} -> do
    shouldProcess <- DO.shouldProcessMessage aggregatorKey msg
    if shouldProcess then do
      sendToIngests state (AggregatorToIngestDataObjectMessage msg)
      sendMessageToPeer state msg
    else pure unit
    pure $ CallReply unit state
  )

dataObjectSendMessage aggregatorKey msg =
  Gen.doCall (serverName aggregatorKey)
  (\state -> do
    shouldProcess <- DO.shouldProcessMessage aggregatorKey msg
    if shouldProcess then do
      sendDownstream state (DataObjectMessage msg)
      sendMessageToPeer state msg
    else pure unit
    pure $ CallReply unit state
  )

dataObjectUpdate :: AggregatorKey -> DO.ObjectUpdateMessage -> Effect Unit
dataObjectUpdate aggregatorKey msg@(DO.ObjectUpdateMessage {operation}) =
  Gen.doCall (serverName aggregatorKey) maybeDoDataObjectUpdate
  where
    maybeDoDataObjectUpdate state = do
      shouldProcess <- DO.shouldProcessMessage aggregatorKey msg
      CallReply unit <$> if shouldProcess then doDataObjectUpdate state
                         else pure state

    doDataObjectUpdate state@{dataObjectState: Left {dataObject: Nothing}} = do
      -- Primary with no data object
      sendResponse state $ DO.Error DO.PendingInitialisation
      pure state

    doDataObjectUpdate state@{dataObjectState: Left dos@{dataObject: Just dataObject}} = do
      -- Primary with data object
      let
        onPrimarySuccess newDataObject = do
          sendResponse state DO.Ok
          sendBroadcast state newDataObject
          sendToBackup dos (P2B_Latest newDataObject)
          pure state{dataObjectState = Left dos{dataObject = Just newDataObject}}
      either (onError state) onPrimarySuccess $ DO.update operation dataObject

    doDataObjectUpdate state@{dataObjectState: Right {dataObject: Nothing}} = do
      -- Backup with no data object
      sendResponse state $ DO.Error DO.PendingInitialisation
      pure state

    doDataObjectUpdate state@{dataObjectState: Right dos@{dataObject: Just (OwnedByUs dataObject)}} = do
      -- Backup with data object.  Given we own it, there can't be a primary
      let
        onBackupSuccess newDataObject = do
          sendResponse state DO.Ok
          sendBroadcast state newDataObject
          pure state{dataObjectState = Right dos{dataObject = Just (OwnedByUs newDataObject)}}
      either (onError state) onBackupSuccess $ DO.update operation dataObject

    doDataObjectUpdate state@{dataObjectState: Right {dataObject: Just (Synchronising _)}} = do
      -- Backup but currently synchronising.  Can't accept updates
      sendResponse state $ DO.Error DO.PendingSynchronisation
      pure state

    doDataObjectUpdate state@{dataObjectState: Right {dataObject: Just (OwnedByPrimary _),
                                                      connectionToPrimary: Nothing}} = do
      -- Backup but owned by primary but no current connection.  Can't accept updates
      sendResponse state $ DO.Error DO.NetworkError
      pure state

    doDataObjectUpdate state@{dataObjectState: Right dos@{dataObject: Just (OwnedByPrimary _),
                                                          connectionToPrimary: Just handler}} = do
      -- Backup but owned by primary - send the update to primary for processing
      sendToPrimary dos (B2P_Update msg)
      pure state

    onError state objectUpdateError = do
      sendResponse state $ DO.Error objectUpdateError
      pure state

    sendResponse state response = do
      responseMsg <- makeResponse msg response
      sendDownstream state (DataObjectUpdateResponse responseMsg)
      sendToIngests state (AggregatorToIngestDataObjectUpdateResponse responseMsg)

makeResponse :: DO.ObjectUpdateMessage -> DO.ObjectUpdateResponse -> Effect DO.ObjectUpdateResponseMessage
makeResponse (DO.ObjectUpdateMessage {sender, senderRef, operation}) response = do
  ref <- Erl.makeRef
  pure $ DO.ObjectUpdateResponseMessage { to: sender
                                        , senderRef
                                        , response
                                        , ref}

sendBroadcast :: State -> DO.Object -> Effect Unit
sendBroadcast state dataObject = do
  ref <- Erl.makeRef
  let
    broadcastMsg = DO.ObjectBroadcastMessage { object: dataObject
                                             , ref: ref}
  sendDownstream state (DataObject broadcastMsg)
  sendToIngests state (AggregatorToIngestDataObject dataObject)

sendDownstream :: State -> DownstreamWsMessage -> Effect Unit
sendDownstream {cachedState: {relays}} msg = do
  void $ traverse doSendMessage $ Map.values relays
  pure unit
  where
    doSendMessage { handler } =
      handler ! WsSend msg

sendToIngests :: State -> AggregatorToIngestWsMessage -> Effect Unit
sendToIngests {cachedState: {localIngests, remoteIngests}} message = do
  void $ traverse doSend $ Map.values localIngests
  void $ traverse doSend $ _.handler <$> Map.values remoteIngests
  where
    doSend handler =
      handler ! WsSend message

startLink :: AggregatorKey -> StreamDetails -> StateServerName -> Effect StartLinkResult
startLink aggregatorKey streamDetails stateServerName = Gen.startLink (serverName aggregatorKey) (init streamDetails stateServerName) handleInfo

stopAction :: AggregatorKey -> Maybe CachedState -> Effect Unit
stopAction aggregatorKey _cachedState = do
  logStop "Ingest Aggregator stopping" {aggregatorKey}
  announceLocalAggregatorStopped aggregatorKey

init :: StreamDetails -> StateServerName -> Effect State
init streamDetails@{role: slotRole, slot: slot@{id: slotId}} stateServerName = do
  logStart "Ingest Aggregator starting" {aggregatorKey, streamDetails}
  _ <- Erl.trapExit true
  config <- Config.ingestAggregatorAgentConfig
  thisServer <- PoPDefinition.getThisServer
  announceLocalAggregatorIsAvailable aggregatorKey (slotDetailsToSlotCharacteristics slot)
  Gen.registerExternalMapping thisServerName (\m -> Workflow <$> workflowMessageMapperImpl m)
  Gen.registerExternalMapping thisServerName (\m -> Gun <$> (WsGun.messageMapper m))
  workflowHandleAndPidsAndSlotConfiguration <- startWorkflowImpl (unwrap streamDetails.slot.id) streamDetails.role $ mkKey <$> streamDetails.slot.profiles
  Gen.registerTerminate thisServerName terminate
  void $ Bus.subscribe thisServerName IntraPoP.bus IntraPoPBus
  let
    Tuple workflowHandle (Tuple webRtcStreamServers (Tuple slotConfiguration _)) = toNested3 workflowHandleAndPidsAndSlotConfiguration
    initialState = { config : config
                   , slotId
                   , slotRole
                   , peerRole: if slotRole == Primary then Backup else Primary
                   , thisServer
                   , aggregatorKey
                   , streamDetails
                   , workflowHandle
                   , webRtcStreamServers
                   , cachedState: emptyCachedState
                   , stateServerName
                   , slotConfiguration
                   , dataObjectState: case slotRole of
                                        Primary -> Left { dataObject: Nothing
                                                        , connectionToBackup: Nothing }
                                        Backup -> Right { dataObject: Nothing
                                                        , connectionToPrimary: Nothing }
                   , maybeStopRef: Nothing
                   }
  cachedState <- fromMaybe emptyCachedState <$> CachedInstanceState.getInstanceData stateServerName
  state2 <- applyCachedState initialState cachedState
  state3 <- attemptConnectionToBackup state2
  pure state3
  where
    mkKey (SlotProfile p) = tuple3 (IngestKey streamDetails.slot.id streamDetails.role p.name) (unwrap p.rtmpStreamName) (unwrap p.name)
    aggregatorKey = streamDetailsToAggregatorKey streamDetails
    thisServerName = (serverName (aggregatorKey))

attemptConnectionToBackup :: State -> Effect State
attemptConnectionToBackup state@{dataObjectState: Left {connectionToBackup: Just _}} = do
  -- No need to attempt connection since we have a connection
  pure $ state

attemptConnectionToBackup state@{slotId, slotRole, dataObjectState: Left dos@{}} = do
  let
    peerKey = AggregatorKey slotId Backup
  mPeerAggregator <- IntraPoP.whereIsIngestAggregator peerKey
  case mPeerAggregator of
    Nothing -> do
      -- No peer; we don't need a peerConnection, we can start our own dataObject
      -- When a peer starts, we'll hear about it from IntraPoP
      pure state{dataObjectState = Left dos{ dataObject = Just DO.new }}

    Just peerAggregator -> do
      let
        peerWsUrl = makeWsUrl peerAggregator $ IngestAggregatorBackupWs slotId Backup
      mPeerWebSocket <- hush <$> WsGun.openWebSocket peerWsUrl
      case mPeerWebSocket of
        Nothing -> do
          -- We have a peer but failed to connect - start timer to retry
          _ <- Timer.sendAfter (serverName (AggregatorKey slotId slotRole)) 1000 AttemptConnectionToBackup
          pure state
        Just socket -> do
          -- We have a peer and have connected - it'll send us its data object...
          pure state{dataObjectState = Left dos{ connectionToBackup = Just socket}}

attemptConnectionToBackup state = do
  -- No need to attempt connection since are backup!
  pure $ state

terminate :: Foreign -> State -> Effect Unit
terminate reason state@{workflowHandle, webRtcStreamServers} = do
  logInfo "Ingest aggregator terminating" {reason}
  stopWorkflowImpl workflowHandle
  _ <- traverse shutdown webRtcStreamServers
  pure unit

emptyCachedState :: CachedState
emptyCachedState = { localIngests: Map.empty
                   , remoteIngests: Map.empty
                   , relays: Map.empty
                   }

hasIngests :: State -> Boolean
hasIngests {cachedState: {localIngests, remoteIngests}} =
  not (Map.isEmpty localIngests && Map.isEmpty remoteIngests)

streamDetailsToAggregatorKey :: StreamDetails -> AggregatorKey
streamDetailsToAggregatorKey streamDetails =
  AggregatorKey streamDetails.slot.id streamDetails.role

handleInfo :: Msg -> State -> Effect (CastResult State)
handleInfo msg state@{aggregatorKey, slotId, stateServerName, workflowHandle, maybeStopRef} =
  case msg of
    AttemptConnectionToBackup -> do
      state2 <- attemptConnectionToBackup state
      pure $ CastNoReply state2

    PrimaryHandlerDown | { dataObjectState: Right dos } <- state -> do
      pure $ CastNoReply state{ dataObjectState = Right dos {connectionToPrimary = Nothing } }

    PrimaryHandlerDown ->
      -- Should never hit this, primary down will always hit backup
      pure $ CastNoReply state

    Gun inMsg ->
      processGunMessage state inMsg

    IntraPoPBus (VmReset server oldRef newRef) -> do
      pure $ CastNoReply state

    IntraPoPBus (IngestAggregatorStarted (AggregatorKey startedId Backup) _) | startedId == slotId -> do
      -- We are primary and backup just started.  Attempt a connection
      state2 <- attemptConnectionToBackup state
      pure $ CastNoReply state2

    IntraPoPBus (IngestAggregatorExited (AggregatorKey exitedId Backup) _) | exitedId == slotId -> do
      CastNoReply <$> case state of
                        { dataObjectState: Left {dataObject: Nothing} } -> do
                          -- We are primary with no object, and backup just exited.  We can create
                          let
                            dataObject = DO.new
                          sendBroadcast state dataObject
                          pure state{dataObjectState = Left { dataObject: Just dataObject
                                                            , connectionToBackup: Nothing}}
                        _ ->
                          -- We are primary and we have an object.  Nothing to do
                          pure state

    IntraPoPBus (IngestAggregatorStarted (AggregatorKey startedId Primary) _) | startedId == slotId -> do
      -- We are backup and primary just started.  Nothing to do.
      pure $ CastNoReply state

    IntraPoPBus (IngestAggregatorExited (AggregatorKey exitedId Primary) _) | exitedId == slotId -> do
      CastNoReply <$> case state of
                        { dataObjectState: Right { dataObject: Nothing } } -> do
                          -- We are backup with no object, and primary just exited.  We can create
                          let
                            dataObject = DO.new
                          sendBroadcast state dataObject
                          pure state{dataObjectState = Right { dataObject: Just (OwnedByUs dataObject)
                                                             , connectionToPrimary: Nothing }}

                        { dataObjectState: Right { dataObject: Just (OwnedByPrimary dataObject) } } ->
                          -- We are backup and currently hold a copy of the primary.  Make it ours
                          pure state{dataObjectState = Right { dataObject: Just (OwnedByUs dataObject)
                                                             , connectionToPrimary: Nothing }}

                        { dataObjectState: Right { dataObject: Just (Synchronising dataObject) } } ->
                          -- We are backup and are currently synchronising. That's not going to happen, so make it ours
                          pure state{dataObjectState =  Right { dataObject: Just (OwnedByUs dataObject)
                                                              , connectionToPrimary: Nothing }}

                        _ ->
                          pure state

    IntraPoPBus (IngestAggregatorStarted _ _) ->
      pure $ CastNoReply state

    IntraPoPBus (IngestAggregatorExited _ _) ->
      pure $ CastNoReply state

    Workflow Noop ->
      pure $ CastNoReply state

    Workflow (RtmpOnFI timestamp pts) -> do
      let
        send relay = relay ! (WsSend $ OnFI {timestamp, pts})
      _ <- traverse send $ _.handler <$> Map.values state.cachedState.relays
      pure $ CastNoReply state

    RelayDown relayServer -> do
      let
        serverAddress = relayServer # unwrap # _.address # unwrap
        maybeExistingPort = view (_relays <<< (at relayServer)) state
      fromMaybe (pure unit) $ deRegisterStreamRelayImpl workflowHandle serverAddress <$> _.port <$> maybeExistingPort
      pure $ CastNoReply (removeRelayFromCachedState relayServer state)

    MaybeStop ref
      | Just ref == maybeStopRef
      , not hasIngests state -> do
        logInfo "Ingest Aggregator stopping" {aggregatorKey}
        pure $ CastStop state
      | otherwise -> pure $ CastNoReply state

    IngestDown profileName ingestAddress isLocal
      | isLocal -> do
        logInfo "Ingest down" {aggregatorKey, profileName}
        state2 <- doRemoveIngest profileName removeLocalIngestFromCachedState state
        pure $ CastNoReply state2
      | otherwise -> do
        logInfo "Ingest down" {aggregatorKey, profileName}
        state2 <- doRemoveIngest profileName removeRemoteIngestFromCachedState state
        pure $ CastNoReply state2

processGunMessage :: State -> WsGun.GunMsg -> Effect (CastResult State)
processGunMessage state@{slotId, slotRole, dataObjectState: Left dos@{connectionToBackup: Just socket}} gunMsg =
  if WsGun.isSocketForMessage gunMsg socket then do
    processResponse <- WsGun.processMessage socket gunMsg
    case processResponse of
      Left error -> do
        _ <- logInfo "Gun process error" {error}
        pure $ CastNoReply state

      Right (WsGun.Internal _) ->
        pure $ CastNoReply state

      Right WsGun.WebSocketUp -> do
        _ <- logInfo "Backup WebSocket up" {slotId}
        WsGun.send socket P2B_Synchronise
        pure $ CastNoReply state

      Right WsGun.WebSocketDown -> do
        _ <- logInfo "Backup WebSocket down" {slotId}
        CastNoReply <$> attemptConnectionToBackup state{dataObjectState = Left dos{connectionToBackup = Nothing}}

      Right (WsGun.Frame (B2P_Message msg)) -> do
        sendDownstream state (DataObjectMessage msg)
        pure $ CastNoReply state

      Right (WsGun.Frame (B2P_Update updateMsg@(DO.ObjectUpdateMessage {operation})))
        | {dataObject: Just dataObject } <- dos ->
        -- We are primary and just got an update msg from backup. Do the update
        let
          onSuccess newDataObject = do
            responseMsg <- makeResponse updateMsg $ DO.Ok
            sendToBackup dos (P2B_UpdateResponse responseMsg)
            sendToBackup dos (P2B_Latest newDataObject)
            sendBroadcast state newDataObject
            pure state{dataObjectState = Left dos{dataObject = Just newDataObject}}
          onError objectUpdateError = do
            responseMsg <- makeResponse updateMsg $ DO.Error objectUpdateError
            sendToBackup dos (P2B_UpdateResponse responseMsg)
            pure state
        in do
          CastNoReply <$> (either onError onSuccess $ DO.update operation dataObject)

      Right (WsGun.Frame (B2P_Update updateMsg)) -> do
        -- We are primary and just got an update msg from backup, but are not in the expected state
        -- Fail the update and log a big warning - shouldn't be able to get here
        _ <- logWarning "DataObject Update message received from backup when we have no data object" {}
        responseMsg <- makeResponse updateMsg $ DO.Error DO.Unexpected
        sendToBackup dos (P2B_UpdateResponse responseMsg)
        pure $ CastNoReply state

      Right (WsGun.Frame (B2P_SynchroniseObject backupDataObject))
        | {dataObject: Just dataObject} <- dos -> do
        -- We are primary and just got an object from backup to synchronise with
        let
          merged = DO.merge dataObject backupDataObject
        sendToBackup dos (P2B_Latest merged)
        pure $ CastNoReply state{dataObjectState = Left dos{dataObject = Just merged}}

      Right (WsGun.Frame (B2P_SynchroniseObject backupDataObject)) -> do
        -- We are primary and just got an object from backup to synchronise with, but we have not object. Simples.
        sendToBackup dos (P2B_Latest backupDataObject)
        pure $ CastNoReply state{dataObjectState = Left dos{dataObject = Just backupDataObject}}

      Right (WsGun.Frame B2P_SynchroniseNoObject)
        | {dataObject: Just dataObject} <- dos -> do
        -- We are primary - just send backup our latest
        sendToBackup dos (P2B_Latest dataObject)
        pure $ CastNoReply state

      Right (WsGun.Frame B2P_SynchroniseNoObject) -> do
        -- We are primary, but we have not object. Simples.
        let
          dataObject = DO.new
        sendToBackup dos (P2B_Latest dataObject)
        pure $ CastNoReply state{dataObjectState = Left dos{dataObject = Just dataObject}}
else
    pure $ CastNoReply state

processGunMessage state gunMsg =
  pure $ CastNoReply state

sendMessageToPeer :: State -> DO.Message -> Effect Unit
sendMessageToPeer state@{dataObjectState: Left primary} msg = do
  sendToBackup primary $ P2B_Message msg

sendMessageToPeer state@{dataObjectState: Right backup} msg = do
  sendToPrimary backup $ B2P_Message msg

sendToPrimary :: BackupDOState -> AggregatorBackupToPrimaryWsMessage -> Effect Unit
sendToPrimary {connectionToPrimary: Nothing} msg = do
  pure unit

sendToPrimary {connectionToPrimary: Just process} msg = do
  process ! (WsSend msg)
  pure unit

sendToBackup :: PrimaryDOState -> AggregatorPrimaryToBackupWsMessage -> Effect Unit
sendToBackup {connectionToBackup: Nothing} msg = do
  pure unit

sendToBackup {connectionToBackup: Just webSocket} msg = do
  WsGun.send webSocket msg
  pure unit


applyCachedState :: State -> CachedState -> Effect State
applyCachedState state {localIngests, remoteIngests, relays} =
  foldWithIndexM (\profileName innerState2 handler -> do
            result <- doAddLocalIngest profileName handler innerState2
            pure $ case result of
                     Left unit -> innerState2
                     Right newState -> newState) state localIngests
  >>= (\innerState -> foldWithIndexM (\profileName innerState2 {ingestAddress, handler} -> do
                                         result <- doAddRemoteIngest profileName handler ingestAddress innerState2
                                         pure $ case result of
                                           Left unit -> innerState2
                                           Right newState -> newState
                                     ) innerState remoteIngests)
  >>= (\ innerState -> foldWithIndexM (\server innerState2 {port, handler} ->
                                        doRegisterRelay {server, port} handler innerState2) innerState relays)

addLocalIngestToCachedState :: ProfileName -> IngestProcess -> State -> State
addLocalIngestToCachedState ingestKey handler = set (_localIngests <<< (at ingestKey)) (Just handler)

addRemoteIngestToCachedState :: ProfileName -> IngestProcess -> ServerAddress -> State -> State
addRemoteIngestToCachedState ingestKey handler ingestAddress = set (_remoteIngests <<< (at ingestKey)) (Just {ingestAddress, handler})

addRelayToCachedState :: (DeliverTo RelayServer) -> RelayProcess -> State -> State
addRelayToCachedState {server, port} handler = set (_relays <<< (at server)) (Just {port, handler})

removeLocalIngestFromCachedState :: ProfileName -> State -> State
removeLocalIngestFromCachedState profileName = set (_localIngests <<< (at profileName)) Nothing

removeRemoteIngestFromCachedState :: ProfileName -> State -> State
removeRemoteIngestFromCachedState profileName = set (_remoteIngests <<< (at profileName)) Nothing

removeRelayFromCachedState :: RelayServer -> State -> State
removeRelayFromCachedState server = set (_relays <<< (at server)) Nothing

updateCachedState :: State -> Effect Unit
updateCachedState state@{ stateServerName
                        , cachedState} =
  CachedInstanceState.recordInstanceData stateServerName cachedState

currentActiveProfiles :: State -> Effect ActiveProfiles
currentActiveProfiles {cachedState: {localIngests, remoteIngests}} = do
  let
    profiles = Map.keys localIngests <> Map.keys remoteIngests
  ref <- Erl.makeRef
  pure $ ActiveProfiles {profiles, ref}

sendActiveProfiles :: State -> Effect Unit
sendActiveProfiles state = do
  activeProfiles <- currentActiveProfiles state
  sendDownstream state (CurrentActiveProfiles activeProfiles)

doAddLocalIngest :: ProfileName -> IngestProcess -> State -> Effect (Either Unit State)
doAddLocalIngest profileName handler state@{thisServer, workflowHandle, slotId, slotRole} =
  let
    isInactive = checkProfileInactive profileName state
  in
    if isInactive then do
      logInfo "Local Ingest added" {slotId, slotRole, profileName}
      let
        state2 = addLocalIngestToCachedState profileName handler state
      updateCachedState state2
      sendActiveProfiles state2
      addLocalIngestImpl workflowHandle (IngestKey slotId slotRole profileName)
      pure $ Right state2
    else do
      logInfo "Local Ingest rejected due to existing ingest" {slotId, slotRole, profileName}
      pure $ Left unit

doAddRemoteIngest :: ProfileName -> IngestProcess -> ServerAddress -> State -> Effect (Either Unit State)
doAddRemoteIngest profileName handler ingestAddress state@{slotId, slotRole, workflowHandle} =
  -- todo - monitor ingest
  let
    isInactive = checkProfileInactive profileName state
  in
    if isInactive then do
      logInfo "Remote ingest added" {slotId, slotRole, profileName, ingestAddress}
      let
        state2 = addRemoteIngestToCachedState profileName handler ingestAddress state
        url = makeUrlAddr ingestAddress (IngestInstanceLlwpE slotId slotRole profileName)
      updateCachedState state2
      sendActiveProfiles state2
      addRemoteIngestImpl workflowHandle (IngestKey slotId slotRole profileName) (unwrap url)
      pure $ Right state2
    else do
      logInfo "Remote Ingest rejected due to existing ingest" {slotId, slotRole, profileName}
      pure $ Left unit

doRegisterRelay :: (DeliverTo RelayServer) -> RelayProcess -> State -> Effect State
doRegisterRelay deliverTo@{server} handler@(Process handlerPid) state@{slotId, slotRole, workflowHandle} = do
  logInfo "Relay added" {slotId, slotRole, deliverTo}
  let
    maybeExistingPort = view (_relays <<< (at server)) state
  deRegisterStreamRelayImpl' maybeExistingPort
  let
    state2 = addRelayToCachedState deliverTo handler state
  updateCachedState state2
  registerStreamRelayImpl workflowHandle serverAddress (deliverTo.port)
  Gen.monitorPid (serverName $ AggregatorKey slotId slotRole) handlerPid (\_ -> RelayDown server)
  pure state2
  where
    serverAddress = server # unwrap # _.address # unwrap
    deRegisterStreamRelayImpl' Nothing = pure unit
    deRegisterStreamRelayImpl' (Just {port}) = deRegisterStreamRelayImpl workflowHandle serverAddress port

doRemoveIngest :: ProfileName -> (ProfileName -> State -> State) -> State -> Effect State
doRemoveIngest profileName cachedStateRemoveFun state@{aggregatorKey, workflowHandle, config:{shutdownLingerTimeMs}, slotId, slotRole} = do
  let
    state2 = cachedStateRemoveFun profileName state
  updateCachedState state2
  sendActiveProfiles state2
  removeIngestImpl workflowHandle (IngestKey slotId slotRole profileName)
  if not hasIngests state2 then do
    ref <- Erl.makeRef
    void $ Timer.sendAfter (serverName aggregatorKey) shutdownLingerTimeMs (MaybeStop ref)
    pure state2{maybeStopRef = Just ref}
  else
    pure state2

--------------------------------------------------------------------------------
-- Lenses
--------------------------------------------------------------------------------
__cachedState :: forall a r. Lens' { cachedState :: a | r } a
__cachedState = prop (SProxy :: SProxy "cachedState")

__localIngests :: forall a r. Lens' { localIngests :: a | r } a
__localIngests = prop (SProxy :: SProxy "localIngests")

__remoteIngests :: forall a r. Lens' { remoteIngests :: a | r } a
__remoteIngests = prop (SProxy :: SProxy "remoteIngests")

__relays :: forall a r. Lens' { relays :: a | r } a
__relays = prop (SProxy :: SProxy "relays")

_localIngests :: Lens' State (Map ProfileName IngestProcess)
_localIngests = __cachedState <<< __localIngests

_remoteIngests :: Lens' State (Map ProfileName { ingestAddress :: ServerAddress
                                               , handler :: IngestProcess })
_remoteIngests = __cachedState <<< __remoteIngests

_relays :: Lens' State (Map RelayServer RegisteredRelay)
_relays = __cachedState <<< __relays

--------------------------------------------------------------------------------
-- Log helpers
--------------------------------------------------------------------------------
domain :: List Atom
domain = atom <$> (show Agent.IngestAggregator : "Instance" : nil)

logInfo :: forall a. Logger (Record a)
logInfo = Logger.doLog domain Logger.info

logWarning :: forall a. Logger (Record a)
logWarning = Logger.doLog domain Logger.warning

logStart :: forall a. Logger (Record a)
logStart = Logger.doLogEvent domain Logger.Start Logger.info

logStop :: forall a. Logger (Record a)
logStop = Logger.doLogEvent domain Logger.Stop Logger.info
