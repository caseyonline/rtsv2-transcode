module Rtsv2.Handler.Relay
       ( startResource
       , registerEgest
       , chainResource
       , stats
       , ChainState
       ) where

import Prelude

import Data.Either (Either(..), hush, isRight)
import Data.Maybe (Maybe, fromMaybe', isNothing)
import Data.Newtype (unwrap)
import Erl.Cowboy.Handlers.Rest (moved, notMoved)
import Erl.Cowboy.Req (StatusCode(..), replyWithoutBody, setHeader)
import Erl.Data.List (singleton, (:))
import Erl.Data.Map as Map
import Rtsv2.Agents.Locator (FailureReason(..), LocalOrRemote(..), LocationResp)
import Rtsv2.Agents.Locator.Relay (findRelayForChain)
import Rtsv2.Agents.StreamRelayInstance (CreateRelayPayload, RegisterEgestPayload, CreateRealyChainPayload)
import Rtsv2.Agents.StreamRelayInstance as StreamRelayInstance
import Rtsv2.Agents.StreamRelayInstanceSup as StreamRelayInstanceSup
import Rtsv2.Handler.MimeType as MimeType
import Rtsv2.PoPDefinition as PoPDefinition
import Rtsv2.Router.Endpoint (Endpoint(..), makeUrl)
import Shared.Types (extractAddress)
import Shared.Types.Agent.State as PublicState
import Shared.Utils (lazyCrashIfMissing)
import Simple.JSON as JSON
import Stetson (HttpMethod(..), StetsonHandler)
import Stetson.Rest as Rest
import StetsonHelper (GenericStetsonGetByStreamId, GenericStetsonHandler, allBody, binaryToString, genericGetByStreamId, genericPost)


stats :: GenericStetsonGetByStreamId PublicState.StreamRelay
stats = genericGetByStreamId StreamRelayInstance.status

startResource :: GenericStetsonHandler CreateRelayPayload
startResource =  genericPost  StreamRelayInstanceSup.startRelay


registerEgest :: GenericStetsonHandler RegisterEgestPayload
registerEgest = genericPost  StreamRelayInstance.registerEgest


newtype ChainState = ChainState { mPayload :: Maybe CreateRealyChainPayload
                                , apiResp  :: LocationResp
                                }


chainResource :: StetsonHandler ChainState
chainResource =
  Rest.handler init
  # Rest.allowedMethods (Rest.result (POST : mempty))
  # Rest.contentTypesAccepted (\req state -> Rest.result (singleton $ MimeType.json acceptJson) req state)
  # Rest.malformedRequest malformedRequest
  # Rest.resourceExists resourceExists
  # Rest.previouslyExisted previouslyExisted
  # Rest.movedTemporarily movedTemporarily
  # Rest.yeeha

  where
    init req = do
      thisServer <- PoPDefinition.getThisServer
      mPayload <- (hush <$> JSON.readJSON <$> binaryToString <$> allBody req mempty)
      let
        req2 = setHeader "x-servedby" (unwrap $ extractAddress thisServer) req
      -- the Left NoResource saves us having to deal with Maybe
      -- it will have a real value in it before we ever need to look for real...
      Rest.initResult req2 $ ChainState {mPayload, apiResp : Left NoResource}

    malformedRequest req state@(ChainState {mPayload}) =
      Rest.result (isNothing mPayload) req state

    acceptJson req (ChainState recState@{mPayload}) = do
      let
        payload = fromMaybe' (lazyCrashIfMissing "impossible payload") mPayload
      apiResp <- findRelayForChain payload
      Rest.result true req (ChainState recState{apiResp = apiResp})

    resourceExists req state@(ChainState {apiResp}) =
      case apiResp of
        Left NotFound ->
          do
            newReq <- replyWithoutBody (StatusCode 404) Map.empty req
            Rest.stop newReq state
        Left NoResource -> do
          newReq <- replyWithoutBody (StatusCode 502) Map.empty req
          Rest.stop newReq state
        Right (Local _)  ->
          Rest.result true req state
        Right (Remote _) ->
          Rest.result false req state

    previouslyExisted req state@(ChainState {apiResp}) =
       Rest.result (isRight apiResp) req state

    movedTemporarily req state@(ChainState {apiResp}) =
      case apiResp of
        Right (Remote server) ->
          let
            url = makeUrl server RelayChainE
          in
            Rest.result (moved $ unwrap url) req state
        _ ->
          Rest.result notMoved req state
