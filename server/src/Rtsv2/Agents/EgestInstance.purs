module Rtsv2.Agents.EgestInstance
  ( startLink
  , stopAction
  , isActive
  , pendingClient
  , addClient
  , currentStats
  , slotConfiguration
  , CreateEgestPayload

  , CachedState
  , DeleteData
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
import Erl.Data.List (List, singleton)
import Erl.Data.Map (values)
import Erl.Process.Raw (Pid)
import Erl.Utils (Ref, makeRef, systemTimeMs)
import Logger (Logger, spy)
import Logger as Logger
import Pinto (ServerName, StartLinkResult)
import Pinto as Pinto
import Pinto.Gen (CallResult(..), CastResult(..))
import Pinto.Gen as Gen
import Pinto.Timer as Timer
import Rtsv2.Agents.CachedInstanceState as CachedInstanceState
import Rtsv2.Agents.EgestInstance.WebRTCTypes (WebRTCStreamServerStats, WebRTCSessionManagerStats)
import Rtsv2.Agents.IntraPoP (IntraPoPBusMessage(..), launchLocalOrRemoteGeneric)
import Rtsv2.Agents.IntraPoP as IntraPoP
import Rtsv2.Agents.Locator.Types (LocalOrRemote(..), RegistrationResp, ResourceResp)
import Rtsv2.Agents.SlotTypes (SlotConfiguration)
import Rtsv2.Agents.StreamRelayInstance as StreamRelayInstance
import Rtsv2.Agents.StreamRelaySup (startRelay) as StreamRelaySup
import Rtsv2.Agents.StreamRelayTypes (CreateRelayPayload, DeRegisterEgestPayload, EgestClientWsMessage(..), EgestServerWsMessage, RegisterEgestPayload)
import Rtsv2.Audit as Audit
import Rtsv2.Config as Config
import Rtsv2.Names as Names
import Rtsv2.PoPDefinition as PoPDefinition
import Rtsv2.Utils (crashIfLeft, noprocToMaybe)
import Shared.Agent as Agent
import Shared.Common (Milliseconds)
import Shared.LlnwApiTypes (StreamIngestProtocol(..))
import Shared.Router.Endpoint (Endpoint(..), makeUrl, makeWsUrl)
import Shared.Rtsv2.JsonLd as JsonLd
import Shared.Stream (AggregatorKey(..), EgestKey(..), RelayKey(..), SlotId, SlotRole)
import Shared.Types (DeliverTo, EgestServer(..), Load, RelayServer, Server, ServerLoad(..))
import Shared.Types.Agent.State as PublicState
import SpudGun (SpudResponse(..))
import SpudGun as SpudGun
import WsGun (GunProtocolResponse(..), GunState, ProcessResponse(..))
import WsGun as WsGun

foreign import startEgestReceiverFFI :: EgestKey -> Effect Int
foreign import getStatsFFI :: EgestKey -> Effect (WebRTCStreamServerStats EgestKey)
foreign import setSlotConfigurationFFI :: EgestKey -> SlotConfiguration -> Effect Unit
foreign import getSlotConfigurationFFI :: EgestKey -> Effect (Maybe SlotConfiguration)

type CreateEgestPayload
  = { slotId :: SlotId
    , slotRole :: SlotRole
    , aggregator :: Server
    }

data DeleteData = LocalDelete DeRegisterEgestPayload (GunState EgestClientWsMessage EgestServerWsMessage)
                | RemoteDelete Server DeRegisterEgestPayload (GunState EgestClientWsMessage EgestServerWsMessage)

type CachedState = DeleteData

type StateServerName = CachedInstanceState.StateServerName CachedState

type State
  = { egestKey :: EgestKey
    , aggregator :: Server
    , thisServer :: EgestServer
    , cachedState :: Maybe CachedState
    , clientCount :: Int
    , lingerTime :: Milliseconds
    , relayCreationRetry :: Milliseconds
    , stopRef :: Maybe Ref
    , receivePortNumber :: Int
    , lastEgestAuditTime :: Milliseconds
    , stateServerName :: StateServerName
    }

payloadToEgestKey :: CreateEgestPayload -> EgestKey
payloadToEgestKey payload = EgestKey payload.slotId payload.slotRole

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
  logInfo "Egest stopping" {egestKey}
  IntraPoP.announceLocalEgestStopped egestKey
  fromMaybe (pure unit) $ deRegisterWithRelay <$> mCachedState
  pure unit

pendingClient :: EgestKey -> Effect RegistrationResp
pendingClient egestKey =
  let
    ourServerName = serverName egestKey
    maybeResetStopTimer state@{clientCount: 0, lingerTime} = do
      ref <- makeRef
      _ <- Timer.sendAfter ourServerName (round $ unwrap lingerTime) (MaybeStop ref)
      pure $ state{ stopRef = Just ref
                  }
    maybeResetStopTimer state =
      pure state
  in
   Gen.doCall ourServerName \state -> do
                                state2 <- maybeResetStopTimer state
                                pure $ CallReply (Right unit) state2

addClient :: Pid -> EgestKey -> Effect RegistrationResp
addClient handlerPid egestKey =
  let
    ourServerName = serverName egestKey
  in
   Gen.doCall ourServerName \state@{clientCount} -> do
     logInfo "Add client" {newCount: clientCount + 1}
     Gen.monitorPid ourServerName handlerPid (\_ -> HandlerDown)
     pure $ CallReply (Right unit) state{ clientCount = clientCount + 1
                                        , stopRef = Nothing
                                        }

slotConfiguration :: EgestKey -> Effect (Maybe SlotConfiguration)
slotConfiguration egestKey =
  getSlotConfigurationFFI egestKey

currentStats :: EgestKey -> Effect PublicState.Egest
currentStats egestKey@(EgestKey slotId slotRole) =
  Gen.call (serverName egestKey) \state@{thisServer, clientCount} ->
    CallReply (JsonLd.egestStatsNode slotId slotRole (wrap (unwrap thisServer)) {clientCount}) state

toEgestServer :: Server -> EgestServer
toEgestServer = unwrap >>> wrap

init :: CreateEgestPayload -> StateServerName -> Effect State
init payload@{slotId, slotRole, aggregator} stateServerName = do
  let
    egestKey = payloadToEgestKey payload
    relayKey = RelayKey slotId slotRole
  receivePortNumber <- startEgestReceiverFFI egestKey
  _ <- Bus.subscribe (serverName egestKey) IntraPoP.bus IntraPoPBus
  logInfo "Egest starting" {payload, receivePortNumber}
  {eqLogIntervalMs, lingerTimeMs, relayCreationRetryMs} <- Config.egestAgentConfig
  now <- systemTimeMs
  thisServer <- PoPDefinition.getThisServer
  _ <- IntraPoP.announceLocalEgestIsAvailable egestKey
  _ <- Timer.sendAfter (serverName egestKey) 0 InitStreamRelays
  _ <- Timer.sendEvery (serverName egestKey) eqLogIntervalMs WriteEqLog

  Gen.registerExternalMapping (serverName egestKey) (\m -> Gun <$> (WsGun.messageMapper m))
  maybeRelay <- IntraPoP.whereIsStreamRelay relayKey
  let
    state ={ egestKey
           , aggregator
           , thisServer : toEgestServer thisServer
           , cachedState : Nothing
           , clientCount : 0
           , lingerTime : wrap $ toNumber lingerTimeMs
           , relayCreationRetry : wrap $ toNumber relayCreationRetryMs
           , stopRef : Nothing
           , receivePortNumber
           , lastEgestAuditTime: now
           , stateServerName
           }
  case maybeRelay of
    Just relay ->
      pure state
    Nothing -> do
      -- Launch
      _ <- StreamRelaySup.startRelay {slotId, slotRole, aggregator}
      pure state

handleInfo :: Msg -> State -> Effect (CastResult State)
handleInfo msg state@{egestKey: egestKey@(EgestKey ourSlotId ourSlotRole), cachedState} =
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

    MaybeStop ref -> maybeStop ref state

    IntraPoPBus (IngestAggregatorExited (AggregatorKey slotId slotRole) serverAddress)
      | slotId == ourSlotId && slotRole == ourSlotRole -> doStop state
      | otherwise -> pure $ CastNoReply state

    IntraPoPBus (VmReset _ _ _) ->
      pure $ CastNoReply state

    Gun inMsg -> do
      case wsState cachedState of
        Just s -> processGunMessage s inMsg
        Nothing -> pure $ CastNoReply state

  where
    wsState Nothing = Nothing
    wsState (Just (LocalDelete _ s)) = Just s
    wsState (Just (RemoteDelete _ _ s)) = Just s

    processGunMessage gunState inMsg = do
      processResponse <- WsGun.processMessage gunState inMsg
      _ <- logInfo "GUN" { inMsg
                         , processResponse
                         }
      case processResponse of
        Left error -> do
          _ <- logInfo "Gun process error" {error}
          pure $ CastNoReply state

        Right (ProtocolResponse Upgrade) -> do
          _ <- WsGun.send gunState (Register {ourSlotId, ourSlotRole})
          pure $ CastNoReply state

        Right (ProtocolResponse _) ->
          pure $ CastNoReply state

        Right (Frame frame) ->
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

maybeStop :: Ref -> State -> Effect (CastResult State)
maybeStop ref state@{ clientCount
                    , stopRef
                    }
  | (clientCount == 0) && (Just ref == stopRef) = doStop state
  | otherwise = pure $ CastNoReply state

doStop :: State -> Effect (CastResult State)
doStop state@{egestKey} = do
  -- Stop actions are all performed in stopAction, which gets called by the CachedState gen_server
  pure $ CastStop state

initStreamRelay :: State -> Effect State
initStreamRelay state@{relayCreationRetry, egestKey: egestKey@(EgestKey slotId slotRole), aggregator, thisServer, stateServerName} = do
  relayResp <- findOrStartRelayForStream state

  case relayResp of
    Left _ ->
      do
        void retryLater
        pure state

    Right localOrRemote ->
      tryConfigureAndRegister localOrRemote

  where
    retryLater = Timer.sendAfter (serverName egestKey) (round $ unwrap relayCreationRetry) InitStreamRelays

    tryConfigureAndRegister localOrRemote =
      do
        deleteData <- registerWithRelay localOrRemote registrationPayload

        maybeConfiguration <- getRelaySlotConfiguration localOrRemote slotId slotRole

        case maybeConfiguration of
          Just configuration ->
            do
              setSlotConfigurationFFI egestKey configuration
              let
                cachedState = deleteData
              CachedInstanceState.recordInstanceData stateServerName cachedState
              pure state{cachedState = Just cachedState}

          Nothing ->
            do
              void retryLater
              pure state

    deliverTo :: DeliverTo EgestServer
    deliverTo = { server: state.thisServer, port: state.receivePortNumber }

    registrationPayload  :: RegisterEgestPayload
    registrationPayload =
      { slotId
      , slotRole
      , deliverTo: deliverTo
      }

registerWithRelay :: (LocalOrRemote Server) -> RegisterEgestPayload -> Effect DeleteData
registerWithRelay (Local _) payload@{slotId, slotRole, deliverTo: {server: Egest {address: egestServerAddress}}} =
  do
    thisServer <- PoPDefinition.getThisServer
    let
      wsUrl = makeWsUrl thisServer $ RelayRegisteredEgestWs slotId slotRole egestServerAddress
    _ <- StreamRelayInstance.registerEgest payload
    wsState <- crashIfLeft =<< WsGun.openWebSocket (spy "XXX HITTING (LOCAL)" wsUrl)
    pure (LocalDelete {slotId, slotRole, egestServerAddress} wsState)
registerWithRelay (Remote remoteServer) payload@{slotId, slotRole, deliverTo: {server: Egest {address: egestServerAddress}}} =
  do
    let
      url = makeUrl remoteServer RelayRegisterEgestE
      wsUrl = makeWsUrl remoteServer $ RelayRegisteredEgestWs slotId slotRole egestServerAddress
    SpudResponse _ _ _ <- crashIfLeft =<< SpudGun.postJson url payload
    wsState <- crashIfLeft =<< WsGun.openWebSocket (spy "XXX HITTING" wsUrl)
    pure (RemoteDelete remoteServer {slotId, slotRole, egestServerAddress} wsState)

deRegisterWithRelay :: DeleteData -> Effect Unit
deRegisterWithRelay (LocalDelete payload _) =
  do
    _ <- noprocToMaybe $ StreamRelayInstance.deRegisterEgest payload
    pure unit
deRegisterWithRelay (RemoteDelete remoteServer {slotId, slotRole, egestServerAddress} _) =
  do
    let
      deleteUrl = makeUrl remoteServer (RelayRegisteredEgestE slotId slotRole egestServerAddress)
    void <$> crashIfLeft =<< SpudGun.delete deleteUrl {}
    pure unit

getRelaySlotConfiguration :: (LocalOrRemote Server) -> SlotId -> SlotRole -> Effect (Maybe SlotConfiguration)
getRelaySlotConfiguration (Local _) slotId slotRole =
  log <$> StreamRelayInstance.slotConfiguration (RelayKey slotId slotRole)

  where
    log = spy "Egest Instance Slot Config from Local Relay"

getRelaySlotConfiguration (Remote remoteServer) slotId slotRole =
  do
    slotConfiguration' <- SpudGun.getJson configURL

    pure $ log $ hush $ (SpudGun.bodyToJSON slotConfiguration')

  where
    configURL = makeUrl remoteServer $ RelaySlotConfigurationE slotId slotRole

    log = spy $ "Egest Instance Slot Config from Remote Relay"


--------------------------------------------------------------------------------
-- Do we have a relay in this pop - yes -> use it
-- Does the stream have an origin (aggregator) - if so build a streamRelay chain to that origin
-- otherwise 404
--------------------------------------------------------------------------------
findOrStartRelayForStream :: State -> Effect (ResourceResp Server)
findOrStartRelayForStream state@{egestKey: EgestKey slotId slotRole, thisServer} = do
  mRelay <- IntraPoP.whereIsStreamRelay $ RelayKey slotId slotRole
  maybe' (\_ -> launchLocalOrRemote state) (pure <<< Right) mRelay


launchLocalOrRemote :: State -> Effect (ResourceResp Server)
launchLocalOrRemote state@{egestKey: egestKey@(EgestKey slotId slotRole), aggregator, thisServer} =
  launchLocalOrRemoteGeneric filterForLoad launchLocal launchRemote
  where
    payload = {slotId, slotRole, aggregator} :: CreateRelayPayload
    launchLocal _ = do
      _ <- StreamRelaySup.startRelay payload
      pure unit
    launchRemote idleServer =
      let
        url = makeUrl idleServer RelayE
      in void <$> crashIfLeft =<< SpudGun.postJson url payload


toRelayServer :: forall a b. Newtype a b => Newtype RelayServer b => a -> RelayServer
toRelayServer = unwrap >>> wrap




loadThresholdToCreateRelay :: Load
loadThresholdToCreateRelay = wrap 50.0

filterForLoad :: ServerLoad -> Boolean
filterForLoad (ServerLoad sl) = sl.load < loadThresholdToCreateRelay


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

logInfo :: forall a. Logger a
logInfo = domainLog Logger.info

-- logWarning :: forall a. Logger a
-- logWarning = domainLog Logger.warning

domainLog :: forall a. Logger {domain :: List Atom, misc :: a} -> Logger a
domainLog = Logger.doLog domain
