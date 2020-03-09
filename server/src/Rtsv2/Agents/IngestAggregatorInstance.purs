module Rtsv2.Agents.IngestAggregatorInstance
  ( startLink
  , isAvailable
  , addLocalIngest
  , addRemoteIngest
  , removeLocalIngest
  , removeRemoteIngest
  , registerRelay
  , getState
  , slotConfiguration

  , PersistentState
  , StateServerName
  , streamDetailsToAggregatorKey
  ) where

import Prelude

import Data.Foldable (foldM)
import Data.FoldableWithIndex (foldWithIndexM)
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
import Erl.Data.Map (Map, delete, insert, size, toUnfoldable)
import Erl.Data.Map as Map
import Erl.Data.Tuple (Tuple2, Tuple3, fst, snd, tuple3)
import Erl.Process.Raw (Pid)
import Erl.Utils (shutdown)
import Erl.Utils as Erl
import Foreign (Foreign)
import Logger (Logger, spy)
import Logger as Logger
import Pinto (ServerName, StartLinkResult, isRegistered)
import Pinto.Gen (CallResult(..), CastResult(..))
import Pinto.Gen as Gen
import Pinto.Timer as Timer
import Rtsv2.Agents.IntraPoP (announceLocalAggregatorIsAvailable)
import Rtsv2.Agents.PersistentInstanceState as PersistentInstanceState
import Rtsv2.Agents.SlotTypes (SlotConfiguration)
import Rtsv2.Agents.StreamRelayTypes (RegisterRelayPayload)
import Rtsv2.Config as Config
import Rtsv2.Names as Names
import Rtsv2.PoPDefinition as PoPDefinition
import Shared.Agent as Agent
import Shared.LlnwApiTypes (SlotProfile(..), StreamDetails)
import Shared.Router.Endpoint (Endpoint(..), makePath)
import Shared.Rtsv2.JsonLd as JsonLd
import Shared.Stream (AggregatorKey(..), IngestKey(..), ProfileName, SlotId(..), SlotRole, ingestKeyToAggregatorKey, ingestKeyToProfileName)
import Shared.Types (DeliverTo, RelayServer, ServerAddress, extractAddress)
import Shared.Types.Agent.State as PublicState

-- TODO: proper type for handle, as done in other places
type WorkflowHandle = Foreign
foreign import startWorkflowImpl :: Int -> Array (Tuple3 IngestKey String String) -> Effect (Tuple2 WorkflowHandle (List Pid))
foreign import stopWorkflowImpl :: WorkflowHandle -> Effect Unit
foreign import addLocalIngestImpl :: WorkflowHandle -> IngestKey -> Effect Unit
foreign import addRemoteIngestImpl :: WorkflowHandle -> IngestKey -> String -> Effect Unit
foreign import removeIngestImpl :: WorkflowHandle -> IngestKey -> Effect Unit
foreign import registerStreamRelayImpl :: WorkflowHandle -> String -> Int -> Effect Unit
foreign import slotConfigurationImpl :: Int -> Effect (Maybe SlotConfiguration)

type PersistentState = { localIngests :: Set IngestKey
                       , remoteIngests :: Map IngestKey ServerAddress
                       , relays :: Set RegisterRelayPayload
                       }

type StateServerName = PersistentInstanceState.StateServerName PersistentState

type State
  = { config :: Config.IngestAggregatorAgentConfig
    , persistentState :: PersistentState
    , thisAddress :: ServerAddress
    , aggregatorKey :: AggregatorKey
    , streamDetails :: StreamDetails
    , activeProfileNames :: Map ProfileName ServerAddress
    , downstreamRelays :: List (DeliverTo RelayServer)
    , workflowHandle :: WorkflowHandle
    , webRtcStreamServers :: List Pid
    , stateServerName :: StateServerName
    }

data Msg
  = Init
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

addLocalIngest :: IngestKey -> Effect Unit
addLocalIngest ingestKey =
  Gen.doCall (serverNameFromIngestKey ingestKey)
  (
    addLocalIngestToPersistentState ingestKey
      <#> (updatePersistentState
           >=> doAddLocalIngest ingestKey
           >>> map (CallReply unit))
  )

addRemoteIngest :: IngestKey -> ServerAddress -> Effect Unit
addRemoteIngest ingestKey remoteServer =
  Gen.doCall (serverNameFromIngestKey ingestKey)
  (
    addRemoteIngestToPersistentState ingestKey remoteServer
    <#> (updatePersistentState
         >=> doAddRemoteIngest ingestKey remoteServer
         >>> map (CallReply unit))
  )

removeLocalIngest :: IngestKey -> Effect Unit
removeLocalIngest ingestKey =
  Gen.doCall (serverName (ingestKeyToAggregatorKey ingestKey))
  (
    removeLocalIngestFromPersistentState ingestKey
    <#> (updatePersistentState
         >=> doRemoveIngest ingestKey
         >>> map (CallReply unit))
  )

removeRemoteIngest :: IngestKey -> Effect Unit
removeRemoteIngest ingestKey =
  Gen.doCall (serverName (ingestKeyToAggregatorKey ingestKey))
  (
    removeRemoteIngestFromPersistentState ingestKey
    <#> (updatePersistentState
         >=> doRemoveIngest ingestKey
         >>> map (CallReply unit))
  )

registerRelay :: RegisterRelayPayload -> Effect Unit
registerRelay payload@{deliverTo} =
  Gen.doCast (serverName $ payloadToAggregatorKey payload)
  (
    addRelayToPersistentState payload
    <#> (updatePersistentState
         >=> doRegisterRelay payload
         >>> map Gen.CastNoReply)
  )

getState :: AggregatorKey -> Effect (PublicState.IngestAggregator List)
getState aggregatorKey@(AggregatorKey slotId slotRole) = Gen.call (serverName aggregatorKey) getState'
  where
    getState' state@{streamDetails, activeProfileNames, downstreamRelays} =
      CallReply { role: slotRole
                , streamDetails
                , activeProfiles: (\(Tuple profileName serverAddress) -> JsonLd.activeIngestLocationNode slotId slotRole profileName serverAddress) <$> (toUnfoldable activeProfileNames)
                , downstreamRelays: (JsonLd.downstreamRelayLocationNode slotId slotRole) <$> downstreamRelays
                }
      state

slotConfiguration :: AggregatorKey -> Effect (Maybe SlotConfiguration)
slotConfiguration (AggregatorKey (SlotId slotId) _streamRole) =
  -- TODO: the key is what the slot config should be keyed on...
  slotConfigurationImpl slotId

startLink :: AggregatorKey -> StreamDetails -> StateServerName -> Effect StartLinkResult
startLink aggregatorKey streamDetails stateServerName = Gen.startLink (serverName aggregatorKey) (init streamDetails stateServerName) handleInfo

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
  _ <- Timer.sendAfter thisServerName 0 Init
  pure { config : config
       , thisAddress : extractAddress thisServer
       , aggregatorKey
       , streamDetails
       , activeProfileNames : Map.empty
       , downstreamRelays : nil
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

streamDetailsToAggregatorKey :: StreamDetails -> AggregatorKey
streamDetailsToAggregatorKey streamDetails =
  AggregatorKey streamDetails.slot.id streamDetails.role

handleInfo :: Msg -> State -> Effect (CastResult State)
handleInfo msg state@{activeProfileNames, aggregatorKey, stateServerName} =
  case msg of
    Init -> do
      persistentState <- fromMaybe emptyPersistentState <$> PersistentInstanceState.getInstanceData stateServerName
      state2 <- applyPersistentState state persistentState
      pure $ CastNoReply state2

    MaybeStop
      | size activeProfileNames == 0 -> do
        logInfo "Ingest Aggregator stopping" {aggregatorKey}
        pure $ CastStop state
      | otherwise -> pure $ CastNoReply state

applyPersistentState :: State -> PersistentState -> Effect State
applyPersistentState state {localIngests, remoteIngests, relays} =
  foldM (flip doAddLocalIngest) state localIngests
  >>= (\innerState -> foldWithIndexM (\ingestKey innerState2 remoteUrl -> doAddRemoteIngest ingestKey remoteUrl innerState2) innerState remoteIngests)
  >>= (\ innerState -> foldM (flip doRegisterRelay) innerState relays)

addLocalIngestToPersistentState :: IngestKey -> State -> State
addLocalIngestToPersistentState ingestKey = set (_localIngests <<< (at ingestKey)) (Just unit)

addRemoteIngestToPersistentState :: IngestKey -> ServerAddress -> State -> State
addRemoteIngestToPersistentState ingestKey remoteServer = set (_remoteIngests <<< (at ingestKey)) (Just remoteServer)

addRelayToPersistentState :: RegisterRelayPayload -> State -> State
addRelayToPersistentState payload = set (_relays <<< (at payload)) (Just unit)

removeLocalIngestFromPersistentState :: IngestKey -> State -> State
removeLocalIngestFromPersistentState ingestKey = set (_localIngests <<< (at ingestKey)) Nothing

removeRemoteIngestFromPersistentState :: IngestKey -> State -> State
removeRemoteIngestFromPersistentState ingestKey = set (_remoteIngests <<< (at ingestKey)) Nothing

removeRelayFromPersistentState :: RegisterRelayPayload -> State -> State
removeRelayFromPersistentState payload = set (_relays <<< (at payload)) Nothing

updatePersistentState :: State -> Effect State
updatePersistentState state@{ stateServerName
                            , persistentState} = do
  PersistentInstanceState.recordInstanceData stateServerName persistentState
  pure $ state

doAddLocalIngest :: IngestKey -> State -> Effect State
doAddLocalIngest ingestKey state@{thisAddress, activeProfileNames, workflowHandle} = do
  logInfo "Ingest added" {ingestKey}
  addLocalIngestImpl workflowHandle ingestKey
  pure state{activeProfileNames = insert (ingestKeyToProfileName ingestKey) thisAddress activeProfileNames}

doAddRemoteIngest :: IngestKey -> ServerAddress -> State -> Effect State
doAddRemoteIngest ingestKey@(IngestKey streamId streamRole profileName) remoteServer state@{activeProfileNames, workflowHandle} = do
  logInfo "Remote ingest added" {ingestKey, source: remoteServer}
  let
    path = makePath (IngestInstanceLlwpE streamId streamRole profileName)
    url = "http://" <> (unwrap remoteServer) <> ":3000" <> path
  addRemoteIngestImpl workflowHandle ingestKey url
  pure $ state{activeProfileNames = insert (ingestKeyToProfileName ingestKey) remoteServer activeProfileNames}

doRegisterRelay :: RegisterRelayPayload -> State -> Effect State
doRegisterRelay payload@{deliverTo} state@{downstreamRelays} = do
  let _ = spy "Registering with ingest aggregator instance" payload
  registerStreamRelayImpl state.workflowHandle (deliverTo.server # unwrap # _.address # unwrap) (deliverTo.port)
  pure $ state{downstreamRelays = deliverTo : downstreamRelays}

doRemoveIngest :: IngestKey -> State -> Effect State
doRemoveIngest ingestKey@(IngestKey _ _ profileName ) state@{activeProfileNames, aggregatorKey, workflowHandle, config:{shutdownLingerTimeMs}} = do
  let
    newActiveProfileNames = delete profileName activeProfileNames
  removeIngestImpl workflowHandle ingestKey
  if (size newActiveProfileNames) == 0 then do
    void $ Timer.sendAfter (serverName aggregatorKey) shutdownLingerTimeMs MaybeStop
    pure unit
  else
    pure unit
  pure $ state{activeProfileNames = newActiveProfileNames}

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

_localIngests :: Lens' State (Set IngestKey)
_localIngests = __persistentState <<< __localIngests

_remoteIngests :: Lens' State (Map IngestKey ServerAddress)
_remoteIngests = __persistentState <<< __remoteIngests

_relays :: Lens' State (Set RegisterRelayPayload)
_relays = __persistentState <<< __relays

--------------------------------------------------------------------------------
-- Log helpers
--------------------------------------------------------------------------------
domains :: List Atom
domains = atom <$> (show Agent.IngestAggregator : "Instance" : nil)

logInfo :: forall a. Logger a
logInfo = domainLog Logger.info

logWarning :: forall a. Logger a
logWarning = domainLog Logger.warning

domainLog :: forall a. Logger {domain :: List Atom, misc :: a} -> Logger a
domainLog = Logger.doLog domains
