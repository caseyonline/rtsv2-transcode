module StetsonHelper
       (
         GetHandler
       , jsonResponse
       , textResponse
       , multiMimeResponse

       , processPostPayload
       , processPostPayloadWithResponse
       , processPostPayloadWithResponseAndUrl
       , PostHandler
       , PostHandlerWithResponse
       , PostHandlerState

       , allBody
       , binaryToString

       , preHookSpyReq
       , preHookSpyReqState
       , preHookSpyState
       ) where

import Prelude

import Data.Either (hush)
import Data.Maybe (Maybe(..), fromMaybe, fromMaybe', isJust, isNothing)
import Data.Newtype (unwrap)
import Effect (Effect)
import Erl.Cowboy.Req (ReadBodyResult(..), Req, readBody, setHeader)
import Erl.Data.Binary (Binary)
import Erl.Data.Binary.IOData (IOData, fromBinary, toBinary)
import Erl.Data.List (List, singleton, (:))
import Erl.Data.Tuple (Tuple2, fst, snd, tuple2)
import Logger (spy)
import Rtsv2.Handler.MimeType as MimeType
import Rtsv2.Utils (noprocToMaybe)
import Shared.Common (Url)
import Shared.Utils (lazyCrashIfMissing)
import Simple.JSON (class ReadForeign, class WriteForeign, writeJSON)
import Simple.JSON as JSON
import Stetson (HttpMethod(..), RestResult, StetsonHandler)
import Stetson.Rest as Rest
import Unsafe.Coerce as Unsafe.Coerce

------------------------------------------------------------------------------
-- GET helpers
------------------------------------------------------------------------------
type GetHandler a = StetsonHandler (Maybe a)

jsonResponse :: forall a. WriteForeign a => Effect (Maybe a) -> GetHandler a
jsonResponse =
  multiMimeResponse (singleton $ MimeType.json writeJSON)

textResponse :: Effect (Maybe String) -> GetHandler String
textResponse =
  multiMimeResponse (singleton $ MimeType.text identity)

multiMimeResponse :: forall a. List (Tuple2 String (a -> String)) -> Effect (Maybe a) -> GetHandler a
multiMimeResponse mimeFormatters getData =
  Rest.handler init
  # Rest.allowedMethods (Rest.result (GET : mempty))
  # Rest.resourceExists (\req state -> do
                          mData <- join <$> noprocToMaybe getData
                          Rest.result (isJust mData) req mData)
  # Rest.contentTypesProvided (\req state ->
                                let
                                  mimeMappings = ((\tuple -> tuple2 (fst tuple) (provideContent (snd tuple))) <$> mimeFormatters)
                                in
                                Rest.result mimeMappings req state)
  # Rest.yeeha
  where
    init req = Rest.initResult (setHeader "access-control-allow-origin" "*" req) Nothing
    provideContent :: (a -> String) -> Req -> Maybe a -> Effect (RestResult String (Maybe a))
    provideContent mimeFormatter req Nothing = Rest.result "" req Nothing
    provideContent mimeFormatter req state@(Just theData) = Rest.result (mimeFormatter theData) req state

------------------------------------------------------------------------------
-- POST helpers
------------------------------------------------------------------------------
type PostHandlerState a b = { mPayload :: Maybe a
                            , mResponse :: Maybe b
                            , mUrl :: Maybe Url}

type PostHandler a = StetsonHandler (PostHandlerState a String)
type PostHandlerWithResponse a b = StetsonHandler (PostHandlerState a b)

processPostPayload :: forall a b. ReadForeign a => (a -> Effect b) -> PostHandler a
processPostPayload proxiedFun =
  processPostPayloadWithResponse $ (map (const (Just ""))) <<< proxiedFun

processPostPayloadWithResponse :: forall a b. ReadForeign a => WriteForeign b => (a -> Effect (Maybe b)) -> PostHandlerWithResponse a b
processPostPayloadWithResponse proxiedFun =
  processPostPayloadWithResponseAndUrl (\a -> ((<$>) (tuple2 Nothing)) <$> (proxiedFun a))

processPostPayloadWithResponseAndUrl :: forall a b. ReadForeign a => WriteForeign b => (a -> Effect (Maybe (Tuple2 (Maybe Url) b))) -> PostHandlerWithResponse a b
processPostPayloadWithResponseAndUrl proxiedFun =
  Rest.handler init
  # Rest.allowedMethods (Rest.result (POST : mempty))
  # Rest.contentTypesAccepted (\req state -> Rest.result (singleton $ MimeType.json acceptJson) req state)
  # Rest.malformedRequest malformedRequest
  # Rest.contentTypesProvided (\req state -> Rest.result (singleton $ MimeType.json provideJson) req state)
  # Rest.yeeha
  where
    init req =
      do
        (mPayload :: Maybe a) <- (hush <$> JSON.readJSON <$> binaryToString <$> allBody req mempty)
        Rest.initResult req { mPayload
                            , mResponse : Nothing
                            , mUrl : Nothing
                            }

    acceptJson req state@{mPayload} = do
      let
        payload = fromMaybe' (lazyCrashIfMissing "impossible noEgestPayload") mPayload
      mResp <- proxiedFun payload
      let
        mUrl = join $ fst <$> mResp
        mResponse = snd <$> mResp
        req2 = fromMaybe req $ ((flip (setHeader "location") req) <$> unwrap <$> mUrl)
      Rest.result true req2 state{ mUrl = mUrl
                                 , mResponse = mResponse
                                 }

    malformedRequest req state@{mPayload} =
      Rest.result (isNothing mPayload) req state

    provideJson req state@{mResponse, mUrl} = do
      let
        response = fromMaybe "" $ JSON.writeJSON <$> mResponse
      Rest.result response req state

--------------------------------------------------------------------------------
-- Body helpers
--------------------------------------------------------------------------------
allBody :: Req -> IOData -> Effect Binary
allBody req acc = do
  readResult <- (readBody req)
  case readResult of
       (FullData body req2) -> pure $ toBinary $ acc <> (fromBinary body)
       (PartialData body req2) -> (allBody req2 $ acc <> (fromBinary body))

binaryToString :: Binary -> String
binaryToString = Unsafe.Coerce.unsafeCoerce


--------------------------------------------------------------------------------
-- Debug helpers
--------------------------------------------------------------------------------
preHookSpyReq :: forall req state. String -> String -> req -> state -> Effect Unit
preHookSpyReq prefix name req _ = hookSpy prefix name {req}

preHookSpyReqState :: forall req state. String -> String -> req -> state -> Effect Unit
preHookSpyReqState prefix name req state = hookSpy prefix name {req, state}

preHookSpyState :: forall req state. String -> String -> req -> state -> Effect Unit
preHookSpyState prefix name _ state = hookSpy prefix name {state}

hookSpy :: forall a. String -> String -> a -> Effect Unit
hookSpy prefix name what =
  let _ = spy (prefix <> "->" <> name) what
  in pure unit
