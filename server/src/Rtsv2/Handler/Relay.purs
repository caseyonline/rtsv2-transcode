module Rtsv2.Handler.Relay
       ( startResource
       , ensureStarted
       , registerEgest
       , registerRelay
       , deRegisterEgest
       , deRegisterRelay
       , registeredEgestWs
       , slotConfiguration
       , stats
       , proxiedStats
       , StartState
       , ProxyState
       , WsMsg
       ) where

import Prelude

import Data.Either (Either(..), hush, isRight)
import Data.Maybe (Maybe(..), fromMaybe', isJust, isNothing, maybe)
import Data.Newtype (unwrap)
import Erl.Atom (atom)
import Erl.Cowboy.Handlers.Rest (moved, notMoved)
import Erl.Cowboy.Req (StatusCode(..), replyWithoutBody, setHeader)
import Erl.Cowboy.Req as Req
import Erl.Data.List (List, singleton, (:))
import Erl.Data.Map as Map
import Logger (spy)
import Rtsv2.Agents.IntraPoP as IntraPoP
import Rtsv2.Agents.Locator.Relay (findOrStart)
import Rtsv2.Agents.Locator.Types (LocalOrRemote(..), NoCapacity(..), ResourceResp, fromLocalOrRemote)
import Rtsv2.Agents.SlotTypes (SlotConfiguration)
import Rtsv2.Agents.StreamRelayInstance as StreamRelayInstance
import Rtsv2.Agents.StreamRelaySup as StreamRelaySup
import Rtsv2.Agents.StreamRelayTypes (CreateRelayPayload, RegisterEgestPayload, RegisterRelayPayload)
import Rtsv2.Handler.MimeType as MimeType
import Rtsv2.PoPDefinition as PoPDefinition
import Shared.Router.Endpoint (Endpoint(..), makeUrl, parseSlotId, parseSlotRole, parseServerAddress)
import Shared.Stream (RelayKey(..), SlotId, SlotRole)
import Shared.Types (Server, ServerAddress, extractAddress)
import Shared.Types.Agent.State (StreamRelay)
import Shared.Utils (lazyCrashIfMissing)
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

registerEgest :: PostHandler RegisterEgestPayload
registerEgest = processPostPayload StreamRelayInstance.registerEgest

registerRelay :: PostHandler RegisterRelayPayload
registerRelay = processPostPayload StreamRelayInstance.registerRelay

deRegisterEgest :: SlotId -> SlotRole -> ServerAddress -> DeleteHandler
deRegisterEgest slotId slotRole egestServerAddress =
  processDelete (Just <$> (StreamRelayInstance.deRegisterEgest {slotId, slotRole, egestServerAddress}))

deRegisterRelay :: SlotId -> SlotRole -> ServerAddress -> DeleteHandler
deRegisterRelay slotId slotRole relayServerAddress =
  processDelete (Just <$> (StreamRelayInstance.deRegisterRelay {slotId, slotRole, relayServerAddress}))

slotConfiguration :: SlotId -> SlotRole -> GetHandler (Maybe SlotConfiguration)
slotConfiguration slotId role =
  jsonResponse $ Just <$> (StreamRelayInstance.slotConfiguration (RelayKey slotId role))

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
  --# Rest.preHook (preHookSpyState "Relay:ensureStarted")
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

type WsState =
  { slotId :: SlotId
  , slotRole :: SlotRole
  , egestAddress :: ServerAddress
  , relayKey :: RelayKey
  }

data WsMsg = Unknown
           | Register Int

registeredEgestWs :: WebSocketHandler WsMsg WsState
registeredEgestWs =
  WebSocket.handler init
  # WebSocket.init (\router state ->
                     let
                       _ = spy "XXX-init router" router
                     in
                      pure $ NoReply state)
  # WebSocket.handle (\frame state ->
                       let
                         _ = spy "XXX-handle frame" frame
                       in
                        pure $ NoReply state)
  # WebSocket.info (\router state ->
                     let
                       _ = spy "XXX-info router" router
                     in
                      pure $ NoReply state)
  --# WebSocket.yeeha - no yeeha...
  where
    init req =
      let
        slotId = fromMaybe' (lazyCrashIfMissing "no slot_id binding") $ parseSlotId =<< Req.binding (atom "slot_id") req
        slotRole = fromMaybe' (lazyCrashIfMissing "no slot_role binding") $ parseSlotRole =<< Req.binding (atom "slot_role") req
        egestAddress = fromMaybe' (lazyCrashIfMissing "no server_address binding") $ parseServerAddress =<< Req.binding (atom "server_address") req
      in
       WebSocket.initResult req { slotId
                                , slotRole
                                , egestAddress
                                , relayKey : RelayKey slotId slotRole
                                }
