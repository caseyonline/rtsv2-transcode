module Rtsv2.Handler.Relay
       ( startResource
       , ensureStarted
       , registerEgest
       , registerRelay
       , stats
       , proxiedStats
       , StartState
       , ProxyState
       ) where

import Prelude

import Data.Either (Either(..), hush, isRight)
import Data.Maybe (Maybe(..), isJust, isNothing, maybe)
import Data.Newtype (unwrap)
import Erl.Cowboy.Handlers.Rest (moved, notMoved)
import Erl.Cowboy.Req (StatusCode(..), replyWithoutBody, setHeader)
import Erl.Data.List (List, singleton, (:))
import Erl.Data.Map as Map
import Rtsv2.Agents.IntraPoP as IntraPoP
import Rtsv2.Agents.Locator.Relay (findOrStart)
import Rtsv2.Agents.Locator.Types (LocalOrRemote(..), NoCapacity(..), ResourceResp, fromLocalOrRemote)
import Rtsv2.Agents.StreamRelay.Instance as StreamRelayInstance
import Rtsv2.Agents.StreamRelay.InstanceSup as StreamRelayInstanceSup
import Rtsv2.Agents.StreamRelay.Types (CreateRelayPayload, RegisterEgestPayload, RegisterRelayPayload)
import Rtsv2.Handler.MimeType as MimeType
import Rtsv2.PoPDefinition as PoPDefinition
import Rtsv2.Router.Endpoint (Endpoint(..), makeUrl)
import Rtsv2.Utils (noprocToMaybe)
import Rtsv2.Web.Bindings as Bindings
import Shared.Stream (RelayKey(..))
import Shared.Types (Server, extractAddress)
import Shared.Types.Agent.State (StreamRelay)
import Shared.Types.Agent.State as PublicState
import Simple.JSON as JSON
import Stetson (HttpMethod(..), StetsonHandler)
import Stetson.Rest as Rest
import StetsonHelper (GenericStatusState(..), GenericStetsonGetByStreamId, GenericStetsonHandler, allBody, binaryToString, genericGetByStreamId, genericPost, genericProvideJson, genericProxyByStreamId, preHookSpyState)


stats :: StetsonHandler (GenericStatusState (StreamRelay List))
stats =  Rest.handler init
  # Rest.allowedMethods (Rest.result (GET : mempty))
  # Rest.contentTypesProvided (\req state -> Rest.result (singleton $ MimeType.json genericProvideJson) req state)
  # Rest.yeeha
  where
    init req =
      let
        streamId = Bindings.streamId req
        streamRole = Bindings.streamRole req
      in do
        mData <- noprocToMaybe $ StreamRelayInstance.status $ RelayKey streamId streamRole
        Rest.initResult req $ GenericStatusState { mData }

startResource :: GenericStetsonHandler CreateRelayPayload
startResource =  genericPost  StreamRelayInstanceSup.startRelay

registerEgest :: GenericStetsonHandler RegisterEgestPayload
registerEgest = genericPost  StreamRelayInstance.registerEgest

registerRelay :: GenericStetsonHandler RegisterRelayPayload
registerRelay = genericPost  StreamRelayInstance.registerRelay



--genericProxyByStreamIdAndRole :: (StreamId -> StreamRole -> Effect (Maybe (LocalOrRemote Server))) -> (StreamId -> Endpoint) -> StetsonHandler GenericProxyState


newtype ProxyState
  = ProxyState { whereIsResp :: Maybe Server
               , relayKey:: RelayKey
               }

proxiedStats =
  Rest.handler init
  # Rest.allowedMethods (Rest.result (GET : mempty))
  # Rest.resourceExists resourceExists
  # Rest.previouslyExisted previouslyExisted
  # Rest.movedTemporarily movedTemporarily

  # Rest.yeeha
  where
    init req =
      let
        streamId = Bindings.streamId req
        streamRole = Bindings.streamRole req
        relayKey = RelayKey streamId streamRole
      in do
        whereIsResp <- (map fromLocalOrRemote) <$> IntraPoP.whereIsStreamRelay relayKey
        Rest.initResult req $
            ProxyState { whereIsResp
                              , relayKey
                              }

    resourceExists req state =
      Rest.result false req state

    previouslyExisted req state@(ProxyState {whereIsResp}) =
      Rest.result (isJust whereIsResp) req state

    movedTemporarily req state@(ProxyState {whereIsResp, relayKey: (RelayKey streamId streamRole)}) =
      case whereIsResp of
        Just server ->
          let
            url = makeUrl server (RelayStatsE streamId streamRole)
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
  # Rest.preHook (preHookSpyState "Relay:ensureStarted")
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


-- chainResource :: StetsonHandler ChainState
-- chainResource =
--   Rest.handler init
--   # Rest.allowedMethods (Rest.result (POST : mempty))
--   # Rest.contentTypesAccepted (\req state -> Rest.result (singleton $ MimeType.json acceptJson) req state)
--   # Rest.malformedRequest malformedRequest
--   # Rest.resourceExists resourceExists
--   # Rest.previouslyExisted previouslyExisted
--   # Rest.movedTemporarily movedTemporarily
--   # Rest.preHook (preHookSpyState "Relay:chainResource")
--   # Rest.yeeha

--   where
--     init req = do
--       thisServer <- PoPDefinition.getThisServer
--       mPayload <- (hush <$> JSON.readJSON <$> binaryToString <$> allBody req mempty)
--       apiResp <- maybe (pure $ Left NoResource) findRelayAndRegisterForChain mPayload

--       let _ = spy "chainResource - init" {mPayload, apiResp}

--       let
--         req2 = setHeader "x-servedby" (unwrap $ extractAddress thisServer) req
--       -- the Left NoResource saves us having to deal with Maybe
--       -- it will have a real value in it before we ever need to look for real...
--       Rest.initResult req2 $ ChainState {mPayload, apiResp}

--     malformedRequest req state@(ChainState {mPayload}) =
--       let _ = spy "malformed" {mPayload} in
--       Rest.result (isNothing mPayload) req state

--     acceptJson req state =
--       let _ = spy "acceptJson" {state} in
--       Rest.result true req state

--     resourceExists req state@(ChainState {apiResp}) =
--       let _ = spy "exists" {state} in
--       case apiResp of
--         Left NotFound ->
--           do
--             newReq <- replyWithoutBody (StatusCode 404) Map.empty req
--             Rest.stop newReq state
--         Left NoResource -> do
--           --TODO - don't think this should be a 502
--           newReq <- replyWithoutBody (StatusCode 502) Map.empty req
--           Rest.stop newReq state
--         Right (Local _)  ->
--           Rest.result true req state
--         Right (Remote _) ->
--           Rest.result false req state

--     previouslyExisted req state@(ChainState {apiResp}) =
--       let _ = spy "previouslyExisted" {state} in
--       Rest.result (isRight apiResp) req state

--     movedTemporarily req state@(ChainState {apiResp}) =
--       let _ = spy "movedTemporarily" {state} in
--       case apiResp of
--         Right (Remote server) ->
--           let
--             url = makeUrl server RelayChainE
--           in
--             let _ = spy "movedTemporarily - to " {url} in
--             Rest.result (moved $ unwrap url) req state
--         _ ->
--           Rest.result notMoved req state
