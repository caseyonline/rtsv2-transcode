module Rtsv2.Agents.IngestAggregatorInstance
  ( startLink
  , stopAction
  , isInstanceAvailable
  , registerIngest
  , registerRelay
  , getState
  , domain
  , dataObjectSendMessage
  , dataObjectUpdate

  , CachedState
  , RegisteredRelay
  , StateServerName
  , streamDetailsToAggregatorKey
  ) where

import Prelude

import Bus as Bus
import Data.Either (Either(..))
import Data.FoldableWithIndex (foldWithIndexM, foldlWithIndex)
import Data.Lens (Lens', set, view)
import Data.Lens.At (at)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..), fromMaybe)
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
import Erl.Utils (shutdown)
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
import Rtsv2.Agents.StreamRelayTypes (AggregatorToIngestWsMessage(..), DownstreamWsMessage(..), WebSocketHandlerMessage(..))
import Rtsv2.Config as Config
import Rtsv2.DataObject (ObjectUpdateMessage(..))
import Rtsv2.DataObject as DO
import Rtsv2.DataObject as DataObject
import Rtsv2.Names as Names
import Rtsv2.PoPDefinition as PoPDefinition
import Shared.Agent as Agent
import Shared.LlnwApiTypes (SlotProfile(..), StreamDetails)
import Shared.Router.Endpoint (Endpoint(..), makeUrlAddr)
import Shared.Rtsv2.JsonLd as JsonLd
import Shared.Stream (AggregatorKey(..), IngestKey(..), ProfileName, SlotId, SlotRole, ingestKeyToAggregatorKey)
import Shared.Types (DeliverTo, RelayServer, Server, ServerAddress, extractAddress)
import Shared.Types.Agent.State as PublicState
import Shared.UUID (UUID)

foreign import data WorkflowHandle :: Type
foreign import startWorkflowImpl :: UUID -> SlotRole -> Array (Tuple3 IngestKey String String) -> Effect (Tuple3 WorkflowHandle (List Pid) SlotConfiguration)
foreign import stopWorkflowImpl :: WorkflowHandle -> Effect Unit
foreign import addLocalIngestImpl :: WorkflowHandle -> IngestKey -> Effect Unit
foreign import addRemoteIngestImpl :: WorkflowHandle -> IngestKey -> String -> Effect Unit
foreign import removeIngestImpl :: WorkflowHandle -> IngestKey -> Effect Unit
foreign import registerStreamRelayImpl :: WorkflowHandle -> String -> Int -> Effect Unit
foreign import deRegisterStreamRelayImpl :: WorkflowHandle -> String -> Int -> Effect Unit
foreign import workflowMessageMapperImpl :: Foreign -> Maybe WorkflowMsg

type RegisteredRelay = { port :: Int
                       , handler :: Process (WebSocketHandlerMessage DownstreamWsMessage)
                       }

type CachedState = { localIngests :: Map ProfileName (Process (WebSocketHandlerMessage AggregatorToIngestWsMessage))
                   , remoteIngests :: Map ProfileName { ingestAddress :: ServerAddress
                                                      , handler :: Process (WebSocketHandlerMessage AggregatorToIngestWsMessage) }
                   , relays :: Map RelayServer RegisteredRelay
                   }

type StateServerName = CachedInstanceState.StateServerName CachedState

type State
  = { config :: Config.IngestAggregatorAgentConfig
    , slotId :: SlotId
    , slotRole :: SlotRole
    , cachedState :: CachedState
    , thisServer :: Server
    , aggregatorKey :: AggregatorKey
    , streamDetails :: StreamDetails
    , workflowHandle :: WorkflowHandle
    , webRtcStreamServers :: List Pid
    , stateServerName :: StateServerName
    , slotConfiguration :: SlotConfiguration
    , dataObject :: DO.Object
    }

data WorkflowMsg = Noop
                 | RtmpOnFI Int Int

data Msg
  = IntraPoPBus IntraPoP.IntraPoPBusMessage
  | MaybeStop
  | RelayDown RelayServer
  | IngestDown ProfileName ServerAddress Boolean
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

registerIngest :: SlotId -> SlotRole -> ProfileName -> ServerAddress -> Process (WebSocketHandlerMessage AggregatorToIngestWsMessage) -> Effect Boolean
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

registerRelay :: SlotId -> SlotRole -> DeliverTo RelayServer -> Process (WebSocketHandlerMessage DownstreamWsMessage) -> Effect SlotConfiguration
registerRelay slotId slotRole deliverTo handler =
  Gen.doCall (serverName $ key)
  (\state@{thisServer, slotConfiguration} -> do
    CallReply slotConfiguration <$> doRegisterRelay deliverTo handler state
  )
  where
    key = AggregatorKey slotId slotRole

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
dataObjectSendMessage aggregatorKey msg@(DO.Message {destination: DO.Publisher
                                                    , msg: payload}) =
  Gen.doCall (serverName aggregatorKey)
  (\state@{cachedState: {localIngests, remoteIngests}} -> do
    shouldProcess <- DataObject.shouldProcessMessage aggregatorKey msg
    if shouldProcess then do
      void $ traverse doSend $ Map.values localIngests
      void $ traverse doSend $ _.handler <$> Map.values remoteIngests
    else pure unit
    pure $ CallReply unit state
  )
  where
    doSend handler =
      handler ! WsSend (AggregatorUpstreamDataObjectMessage payload)

dataObjectSendMessage aggregatorKey msg =
  Gen.doCall (serverName aggregatorKey)
  (\state@{cachedState: {relays}} -> do
    shouldProcess <- DataObject.shouldProcessMessage aggregatorKey msg
    if shouldProcess then void $ traverse doSendMessage $ Map.values relays
    else pure unit
    pure $ CallReply unit state
  )
  where
    doSendMessage { handler } =
      handler ! WsSend (DataObjectMessage msg)

dataObjectUpdate :: AggregatorKey -> DO.ObjectUpdateMessage -> Effect Unit
dataObjectUpdate aggregatorKey msg@(ObjectUpdateMessage {sender, senderRef, operation}) =
  Gen.doCall (serverName aggregatorKey)
  (\state@{dataObject, cachedState: {relays}} -> do
    shouldProcess <- DataObject.shouldProcessMessage aggregatorKey msg
    if shouldProcess then performUpdate dataObject
    else pure unit
    pure $ CallReply unit state
  )
  where
    performUpdate dataObject = pure unit
      -- case DO.update operation dataObject of
      --   Left response

startLink :: AggregatorKey -> StreamDetails -> StateServerName -> Effect StartLinkResult
startLink aggregatorKey streamDetails stateServerName = Gen.startLink (serverName aggregatorKey) (init streamDetails stateServerName) handleInfo

stopAction :: AggregatorKey -> Maybe CachedState -> Effect Unit
stopAction aggregatorKey _cachedState = do
  logStop "Ingest Aggregator stopping" {aggregatorKey}
  announceLocalAggregatorStopped aggregatorKey

init :: StreamDetails -> StateServerName -> Effect State
init streamDetails@{role: slotRole, slot: {id: slotId}} stateServerName = do
  _ <- Erl.trapExit true
  logStart "Ingest Aggregator starting" {aggregatorKey, streamDetails}
  config <- Config.ingestAggregatorAgentConfig
  thisServer <- PoPDefinition.getThisServer
  announceLocalAggregatorIsAvailable aggregatorKey
  Gen.registerExternalMapping thisServerName (\m -> Workflow <$> workflowMessageMapperImpl m)
  workflowHandleAndPidsAndSlotConfiguration <- startWorkflowImpl (unwrap streamDetails.slot.id) streamDetails.role $ mkKey <$> streamDetails.slot.profiles
  Gen.registerTerminate thisServerName terminate
  void $ Bus.subscribe thisServerName IntraPoP.bus IntraPoPBus
  let
    Tuple workflowHandle (Tuple webRtcStreamServers (Tuple slotConfiguration _)) = toNested3 workflowHandleAndPidsAndSlotConfiguration
    initialState = { config : config
                   , slotId
                   , slotRole
                   , thisServer
                   , aggregatorKey
                   , streamDetails
                   , workflowHandle
                   , webRtcStreamServers
                   , cachedState: emptyCachedState
                   , stateServerName
                   , slotConfiguration
                   , dataObject: DO.new
                   }
  cachedState <- fromMaybe emptyCachedState <$> CachedInstanceState.getInstanceData stateServerName
  state2 <- applyCachedState initialState cachedState
  pure state2

  where
    mkKey (SlotProfile p) = tuple3 (IngestKey streamDetails.slot.id streamDetails.role p.name) (unwrap p.rtmpStreamName) (unwrap p.name)
    aggregatorKey = streamDetailsToAggregatorKey streamDetails
    thisServerName = (serverName (aggregatorKey))

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
handleInfo msg state@{aggregatorKey, stateServerName, workflowHandle} =
  case msg of
    IntraPoPBus (VmReset server oldRef newRef) -> do
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

    MaybeStop
      | not hasIngests state -> do
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

addLocalIngestToCachedState :: ProfileName -> Process (WebSocketHandlerMessage AggregatorToIngestWsMessage) -> State -> State
addLocalIngestToCachedState ingestKey handler = set (_localIngests <<< (at ingestKey)) (Just handler)

addRemoteIngestToCachedState :: ProfileName -> Process (WebSocketHandlerMessage AggregatorToIngestWsMessage) -> ServerAddress -> State -> State
addRemoteIngestToCachedState ingestKey handler ingestAddress = set (_remoteIngests <<< (at ingestKey)) (Just {ingestAddress, handler})

addRelayToCachedState :: (DeliverTo RelayServer) -> Process (WebSocketHandlerMessage DownstreamWsMessage) -> State -> State
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

doAddLocalIngest :: ProfileName -> Process (WebSocketHandlerMessage AggregatorToIngestWsMessage) -> State -> Effect (Either Unit State)
doAddLocalIngest profileName handler state@{thisServer, workflowHandle, slotId, slotRole} =
  let
    isInactive = checkProfileInactive profileName state
  in
    if isInactive then do
      logInfo "Local Ingest added" {slotId, slotRole, profileName}
      let
        state2 = addLocalIngestToCachedState profileName handler state
      updateCachedState state2
      addLocalIngestImpl workflowHandle (IngestKey slotId slotRole profileName)
      pure $ Right state2
    else do
      logInfo "Local Ingest rejected due to existing ingest" {slotId, slotRole, profileName}
      pure $ Left unit

doAddRemoteIngest :: ProfileName -> Process (WebSocketHandlerMessage AggregatorToIngestWsMessage) -> ServerAddress -> State -> Effect (Either Unit State)
doAddRemoteIngest profileName handler ingestAddress state@{slotId, slotRole, workflowHandle} =
  let
    isInactive = checkProfileInactive profileName state
  in
    if isInactive then do
      logInfo "Remote ingest added" {slotId, slotRole, profileName, ingestAddress}
      let
        state2 = addRemoteIngestToCachedState profileName handler ingestAddress state
        url = makeUrlAddr ingestAddress (IngestInstanceLlwpE slotId slotRole profileName)
      addRemoteIngestImpl workflowHandle (IngestKey slotId slotRole profileName) (unwrap url)
      pure $ Right state2
    else do
      logInfo "Remote Ingest rejected due to existing ingest" {slotId, slotRole, profileName}
      pure $ Left unit

doRegisterRelay :: (DeliverTo RelayServer) -> Process (WebSocketHandlerMessage DownstreamWsMessage) -> State -> Effect State
doRegisterRelay deliverTo@{server} handler@(Process handlerPid) state@{slotId, slotRole, workflowHandle} = do
  -- todo - monitor handler
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
  removeIngestImpl workflowHandle (IngestKey slotId slotRole profileName)
  if not hasIngests state2 then
    void $ Timer.sendAfter (serverName aggregatorKey) shutdownLingerTimeMs MaybeStop
  else
    pure unit
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

_localIngests :: Lens' State (Map ProfileName (Process (WebSocketHandlerMessage AggregatorToIngestWsMessage)))
_localIngests = __cachedState <<< __localIngests

_remoteIngests :: Lens' State (Map ProfileName { ingestAddress :: ServerAddress
                                               , handler :: Process (WebSocketHandlerMessage AggregatorToIngestWsMessage) })
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
