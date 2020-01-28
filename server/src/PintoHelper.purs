module PintoHelper
       ( exposeState
       , genericPost
       , genericStatus
       , allBody
       , binaryToString
       , GenericHandlerState
       , GenericStatusState
       ) where

import Prelude

import Data.Either (hush)
import Data.Maybe (Maybe(..), fromMaybe', isNothing)
import Data.Newtype (wrap)
import Effect (Effect)
import Erl.Atom (atom)
import Erl.Cowboy.Req (ReadBodyResult(..), Req, StatusCode(..), binding, method, readBody, replyWithoutBody)
import Erl.Data.Binary (Binary)
import Erl.Data.Binary.IOData (IOData, fromBinary, toBinary)
import Erl.Data.List (singleton, (:))
import Erl.Data.Map as Map
import Pinto (ServerName)
import Pinto.Gen (CallResult(..))
import Pinto.Gen as Gen
import Rtsv2.Handler.MimeType as MimeType
import Rtsv2.Utils (noprocToMaybe)
import Shared.Stream (StreamId)
import Shared.Utils (lazyCrashIfMissing)
import Simple.JSON (class ReadForeign, class WriteForeign, writeJSON)
import Simple.JSON as JSON
import Stetson (HttpMethod(..), StetsonHandler)
import Stetson.Rest as Rest
import Unsafe.Coerce as Unsafe.Coerce


exposeState :: forall a state msg. (state -> a) -> ServerName state msg -> Effect a
exposeState exposeFn serverName = Gen.doCall serverName
  \state -> pure $ CallReply (exposeFn state) state

type GenericHandlerState a
  = { mPayload :: Maybe a
    }

genericPost :: forall a b. ReadForeign a => (a -> Effect b) -> StetsonHandler (GenericHandlerState a)
genericPost createFun =
  Rest.handler init
  # Rest.allowedMethods (Rest.result (POST : mempty))
  # Rest.malformedRequest malformedRequest
  # Rest.contentTypesAccepted (\req state -> Rest.result (singleton $ MimeType.json acceptJson) req state)
  # Rest.yeeha
  where
    init req =
      do
        (mPayload :: Maybe a) <- (hush <$> JSON.readJSON <$> binaryToString <$> allBody req mempty)
        Rest.initResult req { mPayload }

    malformedRequest req state@{mPayload} =
      Rest.result (isNothing mPayload) req state

    acceptJson req state@{mPayload} =
      do
        let
          payload = fromMaybe' (lazyCrashIfMissing "impossible noEgestPayload") mPayload
        _ <- createFun payload
        Rest.result true req state

type GenericStatusState a
  = { streamId :: StreamId
    , mStatus :: Maybe a
    }

genericStatus :: forall a. WriteForeign a =>  (StreamId -> Effect a) -> StetsonHandler (GenericStatusState a)
genericStatus statusMethod =
  Rest.handler init
  # Rest.allowedMethods (Rest.result (GET : mempty))
  # Rest.contentTypesProvided (\req state -> Rest.result (singleton $ MimeType.json provideContent) req state)
  # Rest.yeeha
  where
    init req =
      let
        streamId = wrap $ fromMaybe' (lazyCrashIfMissing "stream_id binding missing") $ binding (atom "stream_id") req
        method' = method req
      in do
        mStatus <- noprocToMaybe $ statusMethod streamId
        Rest.initResult req
            { streamId
            , mStatus
            }

    provideContent req state@{mStatus} =
      case mStatus of
        Nothing ->
          do
            newReq <- replyWithoutBody (StatusCode 404) Map.empty req
            Rest.stop newReq state
        Just theStatus ->
          Rest.result (writeJSON theStatus) req state





allBody :: Req -> IOData -> Effect Binary
allBody req acc = do
  readResult <- (readBody req)
  case readResult of
       (FullData body req2) -> pure $ toBinary $ acc <> (fromBinary body)
       (PartialData body req2) -> (allBody req2 $ acc <> (fromBinary body))

binaryToString :: Binary -> String
binaryToString = Unsafe.Coerce.unsafeCoerce
