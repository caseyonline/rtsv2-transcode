module StetsonHelper
       (
         GenericStetsonGet
       , jsonResponse
       , multiMimeResponse
       , textResponse

       , genericPost
       , genericPostWithResponse
       , genericProvideJson
       , GenericStetsonHandler
       , GenericStetsonHandlerWithResponse

       , allBody
       , binaryToString

       , preHookSpyReq
       , preHookSpyReqState
       , preHookSpyState

       , GenericHandlerState
       , GenericStatusState(..)
       , GenericHandlerWithResponseState
       ) where

import Prelude

import Data.Either (hush)
import Data.Maybe (Maybe(..), fromMaybe, fromMaybe', isNothing)
import Effect (Effect)
import Erl.Cowboy.Req (ReadBodyResult(..), Req, StatusCode(..), readBody, replyWithoutBody, setHeader)
import Erl.Data.Binary (Binary)
import Erl.Data.Binary.IOData (IOData, fromBinary, toBinary)
import Erl.Data.List (List, singleton, (:))
import Erl.Data.Map as Map
import Erl.Data.Tuple (Tuple2, fst, snd, tuple2)
import Logger (spy)
import Rtsv2.Handler.MimeType as MimeType
import Rtsv2.Utils (noprocToMaybe)
import Shared.Utils (lazyCrashIfMissing)
import Simple.JSON (class ReadForeign, class WriteForeign, writeJSON)
import Simple.JSON as JSON
import Stetson (HttpMethod(..), RestResult, StetsonHandler)
import Stetson.Rest as Rest
import Unsafe.Coerce as Unsafe.Coerce

------------------------------------------------------------------------------
-- GET helpers
------------------------------------------------------------------------------
type GenericStetsonGet a = StetsonHandler (Maybe a)

jsonResponse :: forall a. WriteForeign a => Effect a -> StetsonHandler (Maybe a)
jsonResponse getData =
  multiMimeResponse $ singleton $ MimeType.json (writeJSON <$> getData)

textResponse :: Effect String -> StetsonHandler (Maybe String)
textResponse getData =
  multiMimeResponse $ singleton $ MimeType.text getData

multiMimeResponse :: forall a. List (Tuple2 String (Effect String)) -> StetsonHandler (Maybe a)
multiMimeResponse getDatas =
  Rest.handler init
  # Rest.allowedMethods (Rest.result (GET : mempty))
  # Rest.contentTypesProvided (\req state -> Rest.result ((\tuple -> tuple2 (fst tuple) (provideContent (snd tuple))) <$> getDatas) req state)
  # Rest.yeeha
  where
    init req = Rest.initResult (setHeader "access-control-allow-origin" "*" req) Nothing
    provideContent getData req state = do
      noprocToMaybe getData >>=
        case _ of
          Nothing ->
            do
              newReq <- replyWithoutBody (StatusCode 404) Map.empty req
              Rest.stop newReq state
          Just theData ->
            Rest.result theData req state

type GenericStetsonHandler a = StetsonHandler (GenericHandlerState a)
newtype GenericHandlerState a = GenericHandlerState { mPayload :: Maybe a }

genericPost :: forall a b. ReadForeign a => (a -> Effect b) -> GenericStetsonHandler a
genericPost proxiedFun =
  Rest.handler init
  # Rest.allowedMethods (Rest.result (POST : mempty))
  # Rest.contentTypesAccepted (\req state -> Rest.result (singleton $ MimeType.json acceptJson) req state)
  # Rest.malformedRequest malformedRequest
  # Rest.yeeha
  where
    init req =
      do
        (mPayload :: Maybe a) <- (hush <$> JSON.readJSON <$> binaryToString <$> allBody req mempty)
        Rest.initResult req $ GenericHandlerState{ mPayload }

    acceptJson req state@(GenericHandlerState {mPayload}) = do
      let
        payload = fromMaybe' (lazyCrashIfMissing "impossible noEgestPayload") mPayload
      _ <- proxiedFun payload
      Rest.result true req state

    malformedRequest req state@(GenericHandlerState {mPayload}) =
      Rest.result (isNothing mPayload) req state

type GenericStetsonHandlerWithResponse a b = StetsonHandler (GenericHandlerWithResponseState a b)
type GenericHandlerWithResponseState a b
  = { mPayload :: Maybe a
    , mResponse :: Maybe b
    }

genericPostWithResponse :: forall a b. ReadForeign a => WriteForeign b => (a -> Effect (Maybe b)) -> GenericStetsonHandlerWithResponse a b
genericPostWithResponse proxiedFun =
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
                            }

    acceptJson req state@{mPayload} = do
      let
        payload = fromMaybe' (lazyCrashIfMissing "impossible noEgestPayload") mPayload
      mResp <- proxiedFun payload
      Rest.result true req state{mResponse = mResp}

    malformedRequest req state@{mPayload} =
      Rest.result (isNothing mPayload) req state

    provideJson req state@{mResponse} = do
      let
        response = fromMaybe "" $ JSON.writeJSON <$> mResponse
      Rest.result response req state


newtype GenericStatusState a = GenericStatusState { mData :: Maybe a }


genericProvideText :: Req -> GenericStatusState String -> Effect (RestResult String (GenericStatusState String))
genericProvideText req state@(GenericStatusState {mData}) =
  case mData of
    Nothing ->
      do
        newReq <- replyWithoutBody (StatusCode 404) Map.empty req
        Rest.stop newReq state
    Just theData ->
      Rest.result theData req state


genericProvideJson :: forall a. WriteForeign a => Req -> GenericStatusState a -> Effect (RestResult String (GenericStatusState a))
genericProvideJson req state@(GenericStatusState {mData}) =
  case mData of
    Nothing ->
      do
        newReq <- replyWithoutBody (StatusCode 404) Map.empty req
        Rest.stop newReq state
    Just theData ->
      Rest.result (writeJSON theData) req state

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
