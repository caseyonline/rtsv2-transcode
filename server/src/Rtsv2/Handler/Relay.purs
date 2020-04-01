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
import Data.Maybe (Maybe(..), fromMaybe, fromMaybe', isJust, isNothing, maybe)
import Data.Newtype (unwrap)
import Effect (Effect)
import Erl.Atom (Atom)
import Erl.Cowboy.Handlers.Rest (moved, notMoved)
import Erl.Cowboy.Handlers.WebSocket (Frame(..))
import Erl.Cowboy.Req (Req, StatusCode(..), replyWithoutBody, setHeader)
import Erl.Data.List (List, singleton, (:))
import Erl.Data.Map as Map
import Erl.Process (Process(..))
import Erl.Utils as Erl
import Foreign (Foreign)
import Logger (spy)
import Logger as Logger
import Rtsv2.Agents.IntraPoP as IntraPoP
import Rtsv2.Agents.Locator.Relay (findOrStart)
import Rtsv2.Agents.Locator.Types (LocalOrRemote(..), ResourceFailed(..), ResourceResp, fromLocalOrRemote)
import Rtsv2.Agents.StreamRelayInstance as StreamRelayInstance
import Rtsv2.Agents.StreamRelaySup as StreamRelaySup
import Rtsv2.Agents.StreamRelayTypes (CreateRelayPayload, DownstreamWsMessage(..), EgestClientWsMessage, RelayToRelayClientWsMessage, WebSocketHandlerMessage(..))
import Rtsv2.Handler.Helper (WebSocketHandlerResult(..), webSocketHandler)
import Rtsv2.Handler.MimeType as MimeType
import Rtsv2.Names as Names
import Rtsv2.PoPDefinition as PoPDefinition
import Shared.Router.Endpoint (Endpoint(..), makeUrl)
import Shared.Stream (RelayKey(..), SlotId, SlotRole)
import Shared.Types (EgestServer(..), RelayServer(..), Server, ServerAddress, ServerLocation(..), SourceRoute, extractAddress)
import Shared.Types.Agent.State (StreamRelay)
import Shared.Utils (lazyCrashIfMissing)
import Simple.JSON (class ReadForeign, class WriteForeign, readJSON, writeJSON)
import Simple.JSON as JSON
import Stetson (HttpMethod(..), InnerStetsonHandler, StetsonHandler)
import Stetson.Rest as Rest
import Stetson.Types (WebSocketCallResult(..))
import Stetson.WebSocket as WebSocket
import StetsonHelper (GetHandler, PostHandler, allBody, binaryToString, jsonResponse, processPostPayload)

stats :: SlotId -> SlotRole -> GetHandler (StreamRelay List)
stats slotId slotRole = jsonResponse $ Just <$> (StreamRelayInstance.status $ RelayKey slotId slotRole)

startResource :: PostHandler CreateRelayPayload
startResource = processPostPayload StreamRelaySup.startRelay

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
      whereIsResp <- (map fromLocalOrRemote) <$> IntraPoP.whereIsStreamRelay relayKey
      Rest.initResult req $
          ProxyState { whereIsResp
                     }

    resourceExists req state =
      Rest.result false req state

    previouslyExisted req state@(ProxyState {whereIsResp}) =
      Rest.result (isJust whereIsResp) req state

    movedTemporarily req state@(ProxyState {whereIsResp}) =
      case whereIsResp of
        Just server ->
          let
            url = makeUrl server (RelayStatsE slotId slotRole)
          in
            Rest.result (moved $ unwrap url) req state
        _ ->
          Rest.result notMoved req state



newtype StartState = StartState { mPayload :: Maybe CreateRelayPayload
                                , apiResp  :: ResourceResp Server
                                }

ensureStarted :: StetsonHandler StartState
ensureStarted =
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
      apiResp <- maybe (pure $ Left NoCapacity) findOrStart mPayload

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
          --TODO - don't think this should be a 502
          newReq <- replyWithoutBody (StatusCode 502) Map.empty req
          Rest.stop newReq state
        Left LaunchFailed -> do
          --TODO - don't think this should be a 502
          newReq <- replyWithoutBody (StatusCode 502) Map.empty req
          Rest.stop newReq state
        Right (Local _)  ->
          Rest.result true req state
        Right (Remote _) ->
          Rest.result false req state

    previouslyExisted req state@(StartState {apiResp}) =
      Rest.result (isRight apiResp) req state

    movedTemporarily req state@(StartState {apiResp}) =
      case apiResp of
        Right (Remote server) ->
          let
            url = makeUrl server RelayEnsureStartedE
          in
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
        ServerLocation {pop, region} = fromMaybe' (lazyCrashIfMissing "unknown server") mServerLocation
      pure { relayServer: Relay { address: relayAddress
                                , pop
                                , region
                                }
           , relayKey: RelayKey slotId slotRole
           }

    wsInit state@{relayServer, relayKey} = do
      self <- Process <$> Erl.self :: Effect (Process (WebSocketHandlerMessage DownstreamWsMessage))
      Erl.monitor (Names.streamRelayInstanceStateName relayKey)
      maybeSlotConfiguration <- StreamRelayInstance.registerRelay slotId slotRole relayServer relayPort sourceRoute self
      pure $ case maybeSlotConfiguration of
               Nothing -> WebSocketNoReply state
               Just slotConfiguration -> WebSocketReply (SlotConfig slotConfiguration) state

    handle :: WsRelayState -> RelayToRelayClientWsMessage -> Effect (WebSocketHandlerResult DownstreamWsMessage WsRelayState)
    handle state _ = do
      pure $ WebSocketNoReply state

    info state WsStop =
      pure $ WebSocketStop state
    info state (WsSend msg) =
      pure $ WebSocketReply msg state

type WsEgestState =
  { egestServer :: EgestServer
  , relayKey :: RelayKey
  }

registeredEgestWs :: SlotId -> SlotRole -> ServerAddress -> Int -> InnerStetsonHandler (WebSocketHandlerMessage DownstreamWsMessage) WsEgestState
registeredEgestWs slotId slotRole egestAddress egestPort =
  webSocketHandler init wsInit handle info
  where
    init req = do
      mServerLocation <- PoPDefinition.whereIsServer egestAddress
      let
        ServerLocation {pop, region} = fromMaybe' (lazyCrashIfMissing "unknown server") mServerLocation
      pure { egestServer: Egest { address: egestAddress
                                , pop
                                , region
                                }
           , relayKey: RelayKey slotId slotRole
           }

    wsInit state@{egestServer, relayKey} = do
      self <- Process <$> Erl.self :: Effect (Process (WebSocketHandlerMessage DownstreamWsMessage))
      Erl.monitor (Names.streamRelayInstanceStateName relayKey)
      maybeSlotConfiguration <- StreamRelayInstance.registerEgest slotId slotRole egestServer egestPort self
      pure $ case maybeSlotConfiguration of
               Nothing -> WebSocketNoReply state
               Just slotConfiguration -> WebSocketReply (SlotConfig slotConfiguration) state

    handle :: WsEgestState -> EgestClientWsMessage -> Effect (WebSocketHandlerResult DownstreamWsMessage WsEgestState)
    handle state _ = do
      pure $ WebSocketNoReply state

    info state WsStop = do
      _ <- logInfo StreamRelayInstance.domain "Agent closed - closing websocket" {}
      pure $ WebSocketStop state
    info state (WsSend msg) =
      pure $ WebSocketReply msg state

--------------------------------------------------------------------------------
-- Log helpers
--------------------------------------------------------------------------------
logInfo :: forall a. List Atom -> String -> Record a -> Effect Unit
logInfo domain msg misc = Logger.doLog domain Logger.info msg misc
