module Rtsv2.Handler.Relay
       ( startResource
       , ensureStarted
       , registeredRelayWs
       , registeredEgestWs
       , stats
       , proxiedStats
       , StartState
       , ProxyState
       ) where

import Prelude

import Data.Either (Either(..), hush, isRight)
import Data.Maybe (Maybe(..), fromMaybe', isJust, isNothing, maybe)
import Data.Newtype (unwrap)
import Effect (Effect)
import Erl.Atom (Atom)
import Erl.Cowboy.Handlers.Rest (moved, notMoved)
import Erl.Cowboy.Req (StatusCode(..), replyWithoutBody, setHeader)
import Erl.Data.List (List, singleton, (:))
import Erl.Data.Map as Map
import Erl.Process (Process(..))
import Erl.Utils as Erl
import Logger as Logger
import Rtsv2.Agents.IntraPoP as IntraPoP
import Rtsv2.Agents.StreamRelayInstance as StreamRelayInstance
import Rtsv2.Agents.StreamRelaySup (findOrStart)
import Rtsv2.Agents.StreamRelaySup as StreamRelaySup
import Rtsv2.Agents.StreamRelayTypes (CreateRelayPayload, DownstreamWsMessage(..), EgestUpstreamWsMessage(..), RelayUpstreamWsMessage(..), WebSocketHandlerMessage(..))
import Rtsv2.Config (LoadConfig)
import Rtsv2.Handler.Helper (WebSocketHandlerResult(..), webSocketHandler)
import Rtsv2.Handler.MimeType as MimeType
import Rtsv2.Names as Names
import Rtsv2.PoPDefinition as PoPDefinition
import Rtsv2.Types (LocalOrRemote(..), ResourceFailed(..), ResourceResp)
import Rtsv2.Utils (noprocToMaybe)
import Shared.Rtsv2.Agent.State (StreamRelay)
import Shared.Rtsv2.Router.Endpoint.Support as Support
import Shared.Rtsv2.Router.Endpoint.System as System
import Shared.Rtsv2.Stream (RelayKey(..), SlotId, SlotRole)
import Shared.Rtsv2.Types (EgestServer(..), OnBehalfOf(..), RelayServer(..), Server(..), ServerAddress, SourceRoute, extractAddress)
import Shared.Utils (lazyCrashIfMissing)
import Simple.JSON as JSON
import Stetson (HttpMethod(..), InnerStetsonHandler, StetsonHandler)
import Stetson.Rest as Rest
import StetsonHelper (GetHandler, PostHandler, allBody, binaryToString, jsonResponse, processPostPayload)

stats :: SlotId -> SlotRole -> GetHandler (StreamRelay List)
stats slotId slotRole = jsonResponse $ Just <$> (StreamRelayInstance.status $ RelayKey slotId slotRole)

startResource :: LoadConfig -> PostHandler CreateRelayPayload
startResource loadConfig = processPostPayload $ StreamRelaySup.startLocalStreamRelay loadConfig RemoteAgent

newtype ProxyState
  = ProxyState { whereIsResp :: Maybe Server
               }

proxiedStats :: SlotId -> SlotRole -> StetsonHandler ProxyState
proxiedStats slotId slotRole =
  Rest.handler init
  # Rest.allowedMethods (Rest.result (GET : mempty))
  # Rest.resourceExists resourceExists
  # Rest.previouslyExisted previouslyExisted
  # Rest.movedTemporarily movedTemporarily

  # Rest.yeeha
  where
    init req = do
      let relayKey = RelayKey slotId slotRole
      whereIsResp <- IntraPoP.whereIsStreamRelay relayKey
      Rest.initResult req $
          ProxyState { whereIsResp
                     }

    resourceExists req state =
      Rest.result false req state

    previouslyExisted req state@(ProxyState {whereIsResp}) =
      Rest.result (isJust whereIsResp) req state

    movedTemporarily req state@(ProxyState {whereIsResp}) =
      case whereIsResp of
        Just server -> do
          url <- Support.makeUrl server (Support.RelayStatsE slotId slotRole)
          Rest.result (moved $ unwrap url) req state
        _ ->
          Rest.result notMoved req state



newtype StartState = StartState { mPayload :: Maybe CreateRelayPayload
                                , apiResp  :: ResourceResp Server
                                }

ensureStarted :: LoadConfig -> StetsonHandler StartState
ensureStarted loadConfig =
  Rest.handler init
  # Rest.allowedMethods (Rest.result (POST : mempty))
  # Rest.contentTypesAccepted (\req state -> Rest.result (singleton $ MimeType.json acceptJson) req state)
  # Rest.malformedRequest malformedRequest
  # Rest.resourceExists resourceExists
  # Rest.previouslyExisted previouslyExisted
  # Rest.movedTemporarily movedTemporarily
--  # Rest.preHook (preHookSpyState "Relay:ensureStarted")
  # Rest.yeeha

  where
    init req = do
      thisServer <- PoPDefinition.getThisServer
      mPayload <- (hush <$> JSON.readJSON <$> binaryToString <$> allBody req mempty)
      apiResp <- maybe (pure $ Left NoCapacity) (findOrStart loadConfig RemoteAgent) mPayload

      let
        req2 = setHeader "x-servedby" (unwrap $ extractAddress thisServer) req
      Rest.initResult req2 $ StartState {mPayload, apiResp}

    malformedRequest req state@(StartState {mPayload}) =
      Rest.result (isNothing mPayload) req state

    acceptJson req state =
      Rest.result true req state

    resourceExists req state@(StartState {apiResp}) =
      case apiResp of
        Left NoCapacity -> do
          newReq <- replyWithoutBody (StatusCode 503) Map.empty req
          Rest.stop newReq state
        Left LaunchFailed -> do
          newReq <- replyWithoutBody (StatusCode 503) Map.empty req
          Rest.stop newReq state
        Left InvalidCanaryState -> do
          newReq <- replyWithoutBody (StatusCode 409) Map.empty req
          Rest.stop newReq state
        Left InvalidRunState -> do
          newReq <- replyWithoutBody (StatusCode 409) Map.empty req
          Rest.stop newReq state
        Left AlreadyRunning -> do
          -- Wouldn't actually get this for a relay, but need to handle all the options
          newReq <- replyWithoutBody (StatusCode 409) Map.empty req
          Rest.stop newReq state
        Right (Local _)  ->
          Rest.result true req state
        Right (Remote _) ->
          Rest.result false req state

    previouslyExisted req state@(StartState {apiResp}) =
      Rest.result (isRight apiResp) req state

    movedTemporarily req state@(StartState {apiResp}) =
      case apiResp of
        Right (Remote server) -> do
          url <- System.makeUrl server System.RelayEnsureStartedE
          Rest.result (moved $ unwrap url) req state
        _ ->
          Rest.result notMoved req state

type WsRelayState =
  { relayServer :: RelayServer
  , relayKey :: RelayKey
  }

registeredRelayWs :: SlotId -> SlotRole -> ServerAddress -> Int -> SourceRoute -> InnerStetsonHandler (WebSocketHandlerMessage DownstreamWsMessage) WsRelayState
registeredRelayWs slotId slotRole relayAddress relayPort sourceRoute =
  webSocketHandler init wsInit handle info
  where
    init req = do
      mServerLocation <- PoPDefinition.whereIsServer relayAddress
      let
        Server server = fromMaybe' (lazyCrashIfMissing "unknown server") mServerLocation
      pure { relayServer: Relay server
           , relayKey: RelayKey slotId slotRole
           }

    wsInit state@{relayServer, relayKey} = do
      self <- Process <$> Erl.self :: Effect (Process (WebSocketHandlerMessage DownstreamWsMessage))
      Erl.monitor (Names.streamRelayInstanceStateName relayKey)
      maybeSlotConfiguration <- noprocToMaybe $ StreamRelayInstance.registerRelay slotId slotRole relayServer relayPort sourceRoute self
      pure $ case maybeSlotConfiguration of
               Nothing -> WebSocketStop state
               Just Nothing -> WebSocketNoReply state
               Just (Just slotConfiguration) -> WebSocketReply (SlotConfig slotConfiguration) state

    handle :: WsRelayState -> RelayUpstreamWsMessage -> Effect (WebSocketHandlerResult DownstreamWsMessage WsRelayState)
    handle state@{relayKey} (RelayUpstreamDataObjectMessage msg) = do
      StreamRelayInstance.dataObjectSendMessage relayKey msg
      pure $ WebSocketNoReply state
    handle state@{relayKey} (RelayUpstreamDataObjectUpdateMessage msg) = do
      StreamRelayInstance.dataObjectUpdateSendMessage relayKey msg
      pure $ WebSocketNoReply state
    handle state@{relayServer, relayKey} (RelayAggregateClientCount {count, time}) = do
      StreamRelayInstance.updateRelayAggregateClientCount relayKey relayServer count time
      pure $ WebSocketNoReply state

    info state WsStop =
      pure $ WebSocketStop state
    info state (WsSend msg) =
      pure $ WebSocketReply msg state

type WsEgestState =
  { egestServer :: EgestServer
  , relayKey :: RelayKey
  }

registeredEgestWs :: SlotId -> SlotRole -> ServerAddress -> Int -> Int -> InnerStetsonHandler (WebSocketHandlerMessage DownstreamWsMessage) WsEgestState
registeredEgestWs slotId slotRole egestAddress egestPort secondaryEgestPort =
  webSocketHandler init wsInit handle info
  where
    init req = do
      mServerLocation <- PoPDefinition.whereIsServer egestAddress
      let
        Server server = fromMaybe' (lazyCrashIfMissing "unknown server") mServerLocation
      pure { egestServer: Egest server
           , relayKey: RelayKey slotId slotRole
           }

    wsInit state@{egestServer, relayKey} = do
      self <- Process <$> Erl.self :: Effect (Process (WebSocketHandlerMessage DownstreamWsMessage))
      Erl.monitor (Names.streamRelayInstanceStateName relayKey)
      maybeSlotConfiguration <- noprocToMaybe $ StreamRelayInstance.registerEgest slotId slotRole egestServer egestPort secondaryEgestPort self
      pure $ case maybeSlotConfiguration of
               Nothing -> WebSocketStop state
               Just Nothing -> WebSocketNoReply state
               Just (Just slotConfiguration) -> WebSocketReply (SlotConfig slotConfiguration) state

    handle :: WsEgestState -> EgestUpstreamWsMessage -> Effect (WebSocketHandlerResult DownstreamWsMessage WsEgestState)
    handle state@{relayKey} (EdgeToRelayDataObjectMessage msg) = do
      StreamRelayInstance.dataObjectSendMessage relayKey msg
      pure $ WebSocketNoReply state
    handle state@{relayKey} (EdgeToRelayDataObjectUpdateMessage msg) = do
      StreamRelayInstance.dataObjectUpdateSendMessage relayKey msg
      pure $ WebSocketNoReply state
    handle state@{egestServer, relayKey} (EgestClientCount count) = do
      StreamRelayInstance.updateEgestClientCount relayKey egestServer count
      pure $ WebSocketNoReply state

    info state WsStop = do
      logInfo StreamRelayInstance.domain "Agent closed - closing websocket" {}
      pure $ WebSocketStop state
    info state (WsSend msg) =
      pure $ WebSocketReply msg state

--------------------------------------------------------------------------------
-- Log helpers
--------------------------------------------------------------------------------
logInfo :: forall report. (List Atom) -> String -> { | report } -> Effect Unit
logInfo domain = Logger.info <<< Logger.traceMetadata domain
