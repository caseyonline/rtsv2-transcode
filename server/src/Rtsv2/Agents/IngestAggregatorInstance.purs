module Rtsv2.Agents.IngestAggregatorInstance
  ( startLink
  , stopAction
  , isAvailable
  , addLocalIngest
  , addRemoteIngest
  , removeLocalIngest
  , removeRemoteIngest
  , registerRelay
  , getState
  , slotConfiguration
  , domain
  , RemoteIngestPayload

  , PersistentState
  , StateServerName
  , streamDetailsToAggregatorKey
  ) where

import Prelude

import Bus as Bus
import Data.Either (Either(..))
import Data.Foldable (foldM, foldl)
import Data.FoldableWithIndex (foldWithIndexM, foldlWithIndex)
import Data.Lens (Lens', set)
import Data.Lens.At (at)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.Set (Set)
import Data.Set as Set
import Data.Symbol (SProxy(..))
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Erl.Atom (Atom, atom)
import Erl.Data.List (List, nil, (:))
import Erl.Data.Map (Map)
import Erl.Data.Map as Map
import Erl.Data.Tuple (Tuple2, Tuple3, fst, snd, tuple3)
import Erl.Process.Raw (Pid)
import Erl.Utils (Ref, shutdown)
import Erl.Utils as Erl
import Foreign (Foreign)
import Logger (Logger, spy)
import Logger as Logger
import Pinto (ServerName, StartLinkResult, isRegistered)
import Pinto.Gen (CallResult(..), CastResult(..))
import Pinto.Gen as Gen
import Pinto.Timer as Timer
import Rtsv2.Agents.IntraPoP (IntraPoPBusMessage(..), announceLocalAggregatorIsAvailable, announceLocalAggregatorStopped, currentRemoteRef)
import Rtsv2.Agents.IntraPoP as IntraPoP
import Rtsv2.Agents.PersistentInstanceState as PersistentInstanceState
import Rtsv2.Agents.SlotTypes (SlotConfiguration)
import Rtsv2.Agents.StreamRelayTypes (RegisterRelayPayload)
import Rtsv2.Config as Config
import Rtsv2.Names as Names
import Rtsv2.PoPDefinition as PoPDefinition
import Shared.Agent as Agent
import Shared.LlnwApiTypes (SlotProfile(..), StreamDetails)
import Shared.Router.Endpoint (Endpoint(..), makeUrl)
import Shared.Rtsv2.JsonLd as JsonLd
import Shared.Stream (AggregatorKey(..), IngestKey(..), ProfileName, SlotId(..), SlotRole, ingestKeyToAggregatorKey)
import Shared.Types (DeliverTo, RelayServer, Server, extractAddress)
import Shared.Types.Agent.State as PublicState
import Shared.UUID (UUID)

-- TODO: proper type for handle, as done in other places
type WorkflowHandle = Foreign
foreign import startWorkflowImpl :: UUID -> Array (Tuple3 IngestKey String String) -> Effect (Tuple2 WorkflowHandle (List Pid))
foreign import stopWorkflowImpl :: WorkflowHandle -> Effect Unit
foreign import addLocalIngestImpl :: WorkflowHandle -> IngestKey -> Effect Unit
foreign import addRemoteIngestImpl :: WorkflowHandle -> IngestKey -> String -> Effect Unit
foreign import removeIngestImpl :: WorkflowHandle -> IngestKey -> Effect Unit
foreign import registerStreamRelayImpl :: WorkflowHandle -> String -> Int -> Effect Unit
foreign import slotConfigurationImpl :: UUID -> Effect (Maybe SlotConfiguration)

type PersistentState = { localIngests :: Set ProfileName
                       , remoteIngests :: Map ProfileName (Tuple Server Ref)
                       , relays :: Set (DeliverTo RelayServer)
                       }

type StateServerName = PersistentInstanceState.StateServerName PersistentState

type RemoteIngestPayload =
  { vmRef :: Ref
  , ingestAddress :: Server
  }

type State
  = { config :: Config.IngestAggregatorAgentConfig
    , slotId :: SlotId
    , slotRole :: SlotRole
    , persistentState :: PersistentState
    , thisServer :: Server
    , aggregatorKey :: AggregatorKey
    , streamDetails :: StreamDetails
    , workflowHandle :: WorkflowHandle
    , webRtcStreamServers :: List Pid
    , stateServerName :: StateServerName
    }

data Msg
  = Init
  | IntraPoPBus IntraPoP.IntraPoPBusMessage
  | MaybeStop

isAvailable :: AggregatorKey -> Effect Boolean
isAvailable aggregatorKey = do
  bool <- isRegistered (serverName aggregatorKey)
  pure bool

payloadToAggregatorKey :: forall r. { slotId :: SlotId, streamRole :: SlotRole | r } -> AggregatorKey
payloadToAggregatorKey payload = AggregatorKey payload.slotId payload.streamRole

serverName :: AggregatorKey -> ServerName State Msg
serverName = Names.ingestAggregatorInstanceName

serverNameFromIngestKey :: IngestKey -> ServerName State Msg
serverNameFromIngestKey = serverName <<< ingestKeyToAggregatorKey

addLocalIngest :: IngestKey -> Effect Boolean
addLocalIngest ingestKey@(IngestKey _ _ profileName) =
  Gen.doCall (serverNameFromIngestKey ingestKey)
  (\state -> do
      result <- doAddLocalIngest profileName state
      case result of
        Left unit ->
          pure $ CallReply false state
        Right state2 ->
          pure $ CallReply true state2
  )

addRemoteIngest :: IngestKey -> RemoteIngestPayload -> Effect Boolean
addRemoteIngest ingestKey@(IngestKey _ _ profileName) payload@{ingestAddress: remoteServer, vmRef: remoteVmRef} =
  Gen.doCall (serverNameFromIngestKey ingestKey)
  (\state -> do
    result <- doAddRemoteIngest profileName remoteServer remoteVmRef state
    case result of
      Left unit ->
        pure $ CallReply false state

      Right state2 ->
        pure $ CallReply true state2
  )

registerRelay :: RegisterRelayPayload -> Effect Unit
registerRelay payload@{deliverTo} =
  Gen.doCast (serverName $ payloadToAggregatorKey payload)
  (
    map Gen.CastNoReply <<< doRegisterRelay deliverTo
  )

checkVmRef :: Ref -> Server -> Effect Boolean
checkVmRef remoteVmRef remoteServer = do
  currentRemoteVmRef <- currentRemoteRef remoteServer
  pure $ fromMaybe false $ ((==) remoteVmRef) <$> currentRemoteVmRef

checkProfileInactive :: ProfileName -> State -> Boolean
checkProfileInactive profileName {persistentState: {localIngests, remoteIngests}} =
  not (Set.member profileName localIngests) && not (Map.member profileName remoteIngests)

removeLocalIngest :: IngestKey -> Effect Unit
removeLocalIngest ingestKey@(IngestKey _ _ profileName) =
  Gen.doCall (serverName (ingestKeyToAggregatorKey ingestKey))
  (\state -> do
    state2 <- doRemoveIngest profileName removeLocalIngestFromPersistentState state
    pure $ CallReply unit state2
  )

removeRemoteIngest :: IngestKey -> Effect Unit
removeRemoteIngest ingestKey@(IngestKey _ _ profileName) =
  Gen.doCall (serverName (ingestKeyToAggregatorKey ingestKey))
  (\state -> do
    state2 <- doRemoveIngest profileName removeRemoteIngestFromPersistentState state
    pure $ CallReply unit state2
  )

getState :: AggregatorKey -> Effect (PublicState.IngestAggregator List)
getState aggregatorKey@(AggregatorKey slotId slotRole) = Gen.call (serverName aggregatorKey) getState'
  where
    getState' state@{streamDetails, persistentState: {localIngests, remoteIngests, relays}, thisServer} =
      CallReply { role: slotRole
                , streamDetails
                , activeProfiles: (\(Tuple profileName server) -> JsonLd.activeIngestLocationNode slotId slotRole profileName (extractAddress server)) <$> allProfiles
                , downstreamRelays: foldl (\acc deliverTo -> (JsonLd.downstreamRelayLocationNode slotId slotRole deliverTo) : acc) nil relays
                }
      state
      where
        localProfiles outerAcc = foldl (\acc profileName -> (Tuple profileName thisServer) : acc) outerAcc localIngests
        remoteProfiles outerAcc = foldlWithIndex (\profileName acc (Tuple remoteServer ref) -> (Tuple profileName remoteServer) : acc) outerAcc remoteIngests
        allProfiles = remoteProfiles $ localProfiles nil


slotConfiguration :: AggregatorKey -> Effect (Maybe SlotConfiguration)
slotConfiguration (AggregatorKey (SlotId slotId) _streamRole) =
  -- TODO: the key is what the slot config should be keyed on...
  slotConfigurationImpl slotId

startLink :: AggregatorKey -> StreamDetails -> StateServerName -> Effect StartLinkResult
startLink aggregatorKey streamDetails stateServerName = Gen.startLink (serverName aggregatorKey) (init streamDetails stateServerName) handleInfo

stopAction :: AggregatorKey -> Maybe PersistentState -> Effect Unit
stopAction aggregatorKey _persistentState =
  announceLocalAggregatorStopped aggregatorKey

init :: StreamDetails -> StateServerName -> Effect State
init streamDetails@{role: slotRole, slot: {id: slotId}} stateServerName = do
  let
    thisServerName = (serverName (streamDetailsToAggregatorKey streamDetails))
  _ <- Erl.trapExit true
  logInfo "Ingest Aggregator starting" {aggregatorKey, streamDetails}
  config <- Config.ingestAggregatorAgentConfig
  thisServer <- PoPDefinition.getThisServer
  announceLocalAggregatorIsAvailable aggregatorKey
  workflowHandleAndPids <- startWorkflowImpl (unwrap streamDetails.slot.id) $ mkKey <$> streamDetails.slot.profiles
  Gen.registerTerminate thisServerName terminate
  void $ Bus.subscribe thisServerName IntraPoP.bus IntraPoPBus
  _ <- Timer.sendAfter thisServerName 0 Init
  pure { config : config
       , slotId
       , slotRole
       , thisServer
       , aggregatorKey
       , streamDetails
       , workflowHandle: fst workflowHandleAndPids
       , webRtcStreamServers: snd workflowHandleAndPids
       , persistentState: emptyPersistentState
       , stateServerName
       }
  where
    mkKey (SlotProfile p) = tuple3 (IngestKey streamDetails.slot.id streamDetails.role p.name) (unwrap p.rtmpStreamName) (unwrap p.name)

    aggregatorKey = streamDetailsToAggregatorKey streamDetails

terminate :: Foreign -> State -> Effect Unit
terminate reason state@{workflowHandle, webRtcStreamServers} = do
  logInfo "Ingest aggregator terminating" {reason}
  stopWorkflowImpl workflowHandle
  _ <- traverse shutdown webRtcStreamServers
  pure unit

emptyPersistentState :: PersistentState
emptyPersistentState = { localIngests: Set.empty
                       , remoteIngests: Map.empty
                       , relays: Set.empty
                       }

hasIngests :: State -> Boolean
hasIngests {persistentState: {localIngests, remoteIngests}} =
  not (Set.isEmpty localIngests && Map.isEmpty remoteIngests)

streamDetailsToAggregatorKey :: StreamDetails -> AggregatorKey
streamDetailsToAggregatorKey streamDetails =
  AggregatorKey streamDetails.slot.id streamDetails.role

handleInfo :: Msg -> State -> Effect (CastResult State)
handleInfo msg state@{aggregatorKey, stateServerName} =
  case msg of
    Init -> do
      persistentState <- fromMaybe emptyPersistentState <$> PersistentInstanceState.getInstanceData stateServerName
      state2 <- applyPersistentState state persistentState
      pure $ CastNoReply state2

    IntraPoPBus (VmReset server oldRef newRef) -> do
      _ <- logInfo "Server reset" {server}
      state2 <- handleServerReset server newRef state
      pure $ CastNoReply state2

    IntraPoPBus (IngestAggregatorExited _ _) ->
      pure $ CastNoReply state

    MaybeStop
      | not hasIngests state -> do
        logInfo "Ingest Aggregator stopping" {aggregatorKey}
        pure $ CastStop state
      | otherwise -> pure $ CastNoReply state

handleServerReset :: Server -> Maybe Ref -> State -> Effect State
handleServerReset resetServer newRef state@{persistentState: {remoteIngests}} =
  foldWithIndexM (\profileName innerState (Tuple server ref) ->
                   if server == resetServer && (Just ref) /= newRef then
                     doRemoveIngest profileName removeRemoteIngestFromPersistentState innerState
                   else
                     pure innerState
                 )
  state
  remoteIngests

applyPersistentState :: State -> PersistentState -> Effect State
applyPersistentState state {localIngests, remoteIngests, relays} =
  foldM (\innerState2 profileName -> do
            result <- doAddLocalIngest profileName innerState2
            pure $ case result of
                     Left unit -> innerState2
                     Right newState -> newState) state localIngests
  >>= (\innerState -> foldWithIndexM (\profileName innerState2 (Tuple remoteServer remoteRef) -> do
                                         result <- doAddRemoteIngest profileName remoteServer remoteRef innerState2
                                         pure $ case result of
                                           Left unit -> innerState2
                                           Right newState -> newState
                                     ) innerState remoteIngests)
  >>= (\ innerState -> foldM (flip doRegisterRelay) innerState relays)

addLocalIngestToPersistentState :: ProfileName -> State -> State
addLocalIngestToPersistentState ingestKey = set (_localIngests <<< (at ingestKey)) (Just unit)

addRemoteIngestToPersistentState :: ProfileName -> Server -> Ref -> State -> State
addRemoteIngestToPersistentState ingestKey remoteServer ref = set (_remoteIngests <<< (at ingestKey)) (Just (Tuple remoteServer ref))

addRelayToPersistentState :: (DeliverTo RelayServer) -> State -> State
addRelayToPersistentState payload = set (_relays <<< (at payload)) (Just unit)

removeLocalIngestFromPersistentState :: ProfileName -> State -> State
removeLocalIngestFromPersistentState profileName = set (_localIngests <<< (at profileName)) Nothing

removeRemoteIngestFromPersistentState :: ProfileName -> State -> State
removeRemoteIngestFromPersistentState profileName = set (_remoteIngests <<< (at profileName)) Nothing

removeRelayFromPersistentState :: DeliverTo RelayServer -> State -> State
removeRelayFromPersistentState payload = set (_relays <<< (at payload)) Nothing

updatePersistentState :: State -> Effect Unit
updatePersistentState state@{ stateServerName
                            , persistentState} =
  PersistentInstanceState.recordInstanceData stateServerName persistentState

doAddLocalIngest :: ProfileName -> State -> Effect (Either Unit State)
doAddLocalIngest profileName state@{thisServer, workflowHandle, slotId, slotRole} =
  let
    isInactive = checkProfileInactive profileName state
  in
    if isInactive then do
      logInfo "Local Ingest added" {slotId, slotRole, profileName}
      let
        state2 = addLocalIngestToPersistentState profileName state
      updatePersistentState state2
      addLocalIngestImpl workflowHandle (IngestKey slotId slotRole profileName)
      pure $ Right state2
    else do
      logInfo "Local Ingest rejected due to existing ingest" {slotId, slotRole, profileName}
      pure $ Left unit

doAddRemoteIngest :: ProfileName -> Server -> Ref -> State -> Effect (Either Unit State)
doAddRemoteIngest profileName remoteServer remoteVmRef state@{slotId, slotRole, workflowHandle} =
  let
    isInactive = checkProfileInactive profileName state
  in
    if isInactive then do
      vmRefOk <- checkVmRef remoteVmRef remoteServer
      if vmRefOk then do
        logInfo "Remote ingest added" {slotId, slotRole, profileName, source: remoteServer}
        let
          state2 = addRemoteIngestToPersistentState profileName remoteServer remoteVmRef state
          url = makeUrl remoteServer (IngestInstanceLlwpE slotId slotRole profileName)
        addRemoteIngestImpl workflowHandle (IngestKey slotId slotRole profileName) (unwrap url)
        pure $ Right state2
      else do
        logInfo "Remote Ingest rejected due to mismatched VM references" {slotId, slotRole, profileName}
        pure $ Left unit
    else do
      logInfo "Remote Ingest rejected due to existing ingest" {slotId, slotRole, profileName}
      pure $ Left unit

doRegisterRelay :: (DeliverTo RelayServer) -> State -> Effect State
doRegisterRelay deliverTo state@{slotId, slotRole} = do
  logInfo "Relay added" {slotId, slotRole, deliverTo}
  let
    state2 = addRelayToPersistentState deliverTo state
  updatePersistentState state2
  registerStreamRelayImpl state.workflowHandle (deliverTo.server # unwrap # _.address # unwrap) (deliverTo.port)
  pure state2

doRemoveIngest :: ProfileName -> (ProfileName -> State -> State) -> State -> Effect State
doRemoveIngest profileName persistentStateRemoveFun state@{aggregatorKey, workflowHandle, config:{shutdownLingerTimeMs}, slotId, slotRole} = do
  let
    state2 = persistentStateRemoveFun profileName state
  updatePersistentState state2
  removeIngestImpl workflowHandle (IngestKey slotId slotRole profileName)
  if not hasIngests state2 then
    void $ Timer.sendAfter (serverName aggregatorKey) shutdownLingerTimeMs MaybeStop
  else
    pure unit
  pure state2

--------------------------------------------------------------------------------
-- Lenses
--------------------------------------------------------------------------------
__persistentState :: forall a r. Lens' { persistentState :: a | r } a
__persistentState = prop (SProxy :: SProxy "persistentState")

__localIngests :: forall a r. Lens' { localIngests :: a | r } a
__localIngests = prop (SProxy :: SProxy "localIngests")

__remoteIngests :: forall a r. Lens' { remoteIngests :: a | r } a
__remoteIngests = prop (SProxy :: SProxy "remoteIngests")

__relays :: forall a r. Lens' { relays :: a | r } a
__relays = prop (SProxy :: SProxy "relays")

_localIngests :: Lens' State (Set ProfileName)
_localIngests = __persistentState <<< __localIngests

_remoteIngests :: Lens' State (Map ProfileName (Tuple Server Ref))
_remoteIngests = __persistentState <<< __remoteIngests

_relays :: Lens' State (Set (DeliverTo RelayServer))
_relays = __persistentState <<< __relays

--------------------------------------------------------------------------------
-- Log helpers
--------------------------------------------------------------------------------
domain :: List Atom
domain = atom <$> (show Agent.IngestAggregator : "Instance" : nil)

logInfo :: forall a. Logger a
logInfo = domainLog Logger.info

logWarning :: forall a. Logger a
logWarning = domainLog Logger.warning

domainLog :: forall a. Logger {domain :: List Atom, misc :: a} -> Logger a
domainLog = Logger.doLog domain
