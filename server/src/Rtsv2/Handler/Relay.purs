module Rtsv2.Handler.Relay
       ( startResource
       , ensureStarted
       -- , registerEgest
       -- , registerRelay
       , deRegisterEgest
       , deRegisterRelay
       , registeredRelayWs
       , registeredEgestWs
       --, slotConfiguration
       , stats
       , proxiedStats
       , StartState
       , ProxyState

       , webSocketHandler
       , WebSocketHandlerResult(..)
       ) where

import Prelude

import Data.Either (Either(..), hush, isRight)
import Data.Maybe (Maybe(..), fromMaybe', isJust, isNothing, maybe)
import Data.Newtype (unwrap)
import Effect (Effect)
import Erl.Atom (atom)
import Erl.Cowboy.Handlers.Rest (moved, notMoved)
import Erl.Cowboy.Handlers.WebSocket (Frame(..))
import Erl.Cowboy.Req (Req, StatusCode(..), replyWithoutBody, setHeader)
import Erl.Cowboy.Req as Req
import Erl.Data.List (List, singleton, (:))
import Erl.Data.Map as Map
import Erl.Process (Process(..))
import Erl.Utils as Erl
import Logger (spy)
import Rtsv2.Agents.IntraPoP as IntraPoP
import Rtsv2.Agents.Locator.Relay (findOrStart)
import Rtsv2.Agents.Locator.Types (LocalOrRemote(..), NoCapacity(..), ResourceResp, fromLocalOrRemote)
import Rtsv2.Agents.StreamRelayInstance as StreamRelayInstance
import Rtsv2.Agents.StreamRelaySup as StreamRelaySup
import Rtsv2.Agents.StreamRelayTypes (CreateRelayPayload, DownstreamWsMessage(..), EgestClientWsMessage, RelayToRelayClientWsMessage, WebSocketHandlerMessage(..))
import Rtsv2.Handler.MimeType as MimeType
import Rtsv2.PoPDefinition as PoPDefinition
import Shared.Router.Endpoint (Endpoint(..), makeUrl, parseSlotId, parseSlotRole, parseServerAddress, parseInt, parseSourceRoute)
import Shared.Stream (RelayKey(..), SlotId, SlotRole)
import Shared.Types (EgestServer(..), RelayServer(..), Server, ServerAddress, ServerLocation(..), SourceRoute, extractAddress)
import Shared.Types.Agent.State (StreamRelay)
import Shared.Utils (lazyCrashIfMissing)
import Simple.JSON (class ReadForeign, class WriteForeign, readJSON, writeJSON)
import Simple.JSON as JSON
import Stetson (HttpMethod(..), StetsonHandler, WebSocketHandler)
import Stetson.Rest as Rest
import Stetson.Types (WebSocketCallResult(..))
import Stetson.WebSocket as WebSocket
import StetsonHelper (DeleteHandler, GetHandler, PostHandler, allBody, binaryToString, jsonResponse, processDelete, processPostPayload)

stats :: SlotId -> SlotRole -> GetHandler (StreamRelay List)
stats slotId slotRole = jsonResponse $ Just <$> (StreamRelayInstance.status $ RelayKey slotId slotRole)

startResource :: PostHandler CreateRelayPayload
startResource = processPostPayload StreamRelaySup.startRelay

-- registerEgest :: PostHandler RegisterEgestPayload
-- registerEgest = processPostPayload StreamRelayInstance.registerEgest

-- registerRelay :: PostHandler RegisterRelayPayload
-- registerRelay = processPostPayload StreamRelayInstance.registerRelay

deRegisterEgest :: SlotId -> SlotRole -> ServerAddress -> DeleteHandler
deRegisterEgest slotId slotRole egestServerAddress =
  processDelete (Just <$> (StreamRelayInstance.deRegisterEgest {slotId, slotRole, egestServerAddress}))

deRegisterRelay :: SlotId -> SlotRole -> ServerAddress -> DeleteHandler
deRegisterRelay slotId slotRole relayServerAddress =
  processDelete (Just <$> (StreamRelayInstance.deRegisterRelay {slotId, slotRole, relayServerAddress}))

-- slotConfiguration :: SlotId -> SlotRole -> GetHandler (Maybe SlotConfiguration)
-- slotConfiguration slotId role =
--   jsonResponse $ Just <$> (StreamRelayInstance.slotConfiguration (RelayKey slotId role))

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
  { slotId :: SlotId
  , slotRole :: SlotRole
  , relayAddress :: ServerAddress
  , relayPort :: Int
  , relayServer :: RelayServer
  , sourceRoute :: SourceRoute
  , relayKey :: RelayKey
  }

registeredRelayWs :: WebSocketHandler WebSocketHandlerMessage WsRelayState
registeredRelayWs =
  webSocketHandler init wsInit handle info
  where
    init req = do
      let
        slotId = fromMaybe' (lazyCrashIfMissing "no slot_id binding") $ parseSlotId =<< Req.binding (atom "slot_id") req
        slotRole = fromMaybe' (lazyCrashIfMissing "no slot_role binding") $ parseSlotRole =<< Req.binding (atom "slot_role") req
        relayAddress = fromMaybe' (lazyCrashIfMissing "no server_address binding") $ parseServerAddress =<< Req.binding (atom "server_address") req
        relayPort = fromMaybe' (lazyCrashIfMissing "no port") $ parseInt =<< Req.binding (atom "port") req
        sourceRoute = fromMaybe' (lazyCrashIfMissing "no source_route") $ parseSourceRoute =<< Req.binding (atom "source_route") req
      mServerLocation <- PoPDefinition.whereIsServer relayAddress
      let
        ServerLocation {pop, region} = fromMaybe' (lazyCrashIfMissing "unknown server") mServerLocation
      pure { slotId
           , slotRole
           , relayAddress
           , relayPort
           , relayServer: Relay { address: relayAddress
                                , pop
                                , region
                                }
           , sourceRoute
           , relayKey: RelayKey slotId slotRole
           }

    wsInit state@{slotId, slotRole, relayServer, relayPort, sourceRoute} = do
      self <- Process <$> Erl.self :: Effect (Process WebSocketHandlerMessage)
      maybeSlotConfiguration <- StreamRelayInstance.registerRelay {slotId, slotRole, deliverTo: {server: relayServer, port: relayPort}, sourceRoute} self
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
  { slotId :: SlotId
  , slotRole :: SlotRole
  , egestAddress :: ServerAddress
  , egestPort :: Int
  , egestServer :: EgestServer
  , relayKey :: RelayKey
  }

registeredEgestWs :: WebSocketHandler WebSocketHandlerMessage WsEgestState
registeredEgestWs =
  webSocketHandler init wsInit handle info
  where
    init req = do
      let
        slotId = fromMaybe' (lazyCrashIfMissing "no slot_id binding") $ parseSlotId =<< Req.binding (atom "slot_id") req
        slotRole = fromMaybe' (lazyCrashIfMissing "no slot_role binding") $ parseSlotRole =<< Req.binding (atom "slot_role") req
        egestAddress = fromMaybe' (lazyCrashIfMissing "no server_address binding") $ parseServerAddress =<< Req.binding (atom "server_address") req
        egestPort = fromMaybe' (lazyCrashIfMissing "no port") $ parseInt =<< Req.binding (atom "port") req
      mServerLocation <- PoPDefinition.whereIsServer egestAddress
      let
        ServerLocation {pop, region} = fromMaybe' (lazyCrashIfMissing "unknown server") mServerLocation
      pure { slotId
           , slotRole
           , egestAddress
           , egestPort
           , egestServer: Egest { address: egestAddress
                                , pop
                                , region
                                }
           , relayKey: RelayKey slotId slotRole
           }

    wsInit state@{slotId, slotRole, egestServer, egestPort} = do
      self <- Process <$> Erl.self :: Effect (Process WebSocketHandlerMessage)
      maybeSlotConfiguration <- StreamRelayInstance.registerEgest {slotId, slotRole, deliverTo: {server: egestServer, port: egestPort}} self --todo: and pid
      pure $ case maybeSlotConfiguration of
               Nothing -> WebSocketNoReply state
               Just slotConfiguration -> WebSocketReply (SlotConfig slotConfiguration) state

    handle :: WsEgestState -> EgestClientWsMessage -> Effect (WebSocketHandlerResult DownstreamWsMessage WsEgestState)
    handle state _ = do
      pure $ WebSocketNoReply state

    info state WsStop =
      pure $ WebSocketStop state
    info state (WsSend msg) =
      pure $ WebSocketReply msg state

-- TODO Largely generic helper - could move into StetsonHelper at some point....
data WebSocketHandlerResult serverMsg state = WebSocketNoReply state
                                            | WebSocketReply serverMsg state
                                            | WebSocketStop state

webSocketHandler :: forall clientMsg serverMsg infoMsg state. ReadForeign clientMsg => WriteForeign serverMsg =>
                    (Req -> Effect state) ->
                    (state -> Effect (WebSocketHandlerResult serverMsg state)) ->
                    (state -> clientMsg -> Effect (WebSocketHandlerResult serverMsg state)) ->
                    (state -> infoMsg -> Effect (WebSocketHandlerResult serverMsg state)) ->
                    WebSocketHandler infoMsg state
webSocketHandler init wsInit handle info =
  WebSocket.handler (\req -> do
                        state <- init req
                        WebSocket.initResult req state
                    )

  # WebSocket.init (\router state -> do
                     res <- wsInit state
                     pure $ dispatchResult res
                   )

  # WebSocket.handle (\rawFrame state ->
                       case rawFrame of
                         TextFrame str ->
                           case readJSON str of
                             Left _ ->
                               -- todo - what about parse failures?  Should crash?
                               pure $ NoReply state
                             Right frame -> do
                               res <- handle state frame
                               pure $ dispatchResult res
                         BinaryFrame bin ->
                           pure $ NoReply state
                         PingFrame bin ->
                           pure $ NoReply state
                         PongFrame bin ->
                           pure $ NoReply state
                     )

  # WebSocket.info (\msg state -> do
                       res <- info state msg
                       pure $ dispatchResult res
                   )
  --# WebSocket.yeeha - no yeeha...
  where
    dispatchResult res =
      case res of
        WebSocketNoReply state ->
          NoReply state
        WebSocketReply response state ->
          let
            str = writeJSON response
          in
            Reply (singleton (TextFrame str)) state
        WebSocketStop state ->
          Stop state
