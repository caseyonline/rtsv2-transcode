module StetsonHelper
       ( genericPost
       , genericPostWithResponse
       , genericGetByStreamId
       , genericGetByPoPName
       , GenericStetsonGetByStreamId
       , GenericStetsonHandler
       , GenericStetsonHandlerWithResponse

       , allBody
       , binaryToString

         -- internal - not for public consumption!
       , Internal_GenericStatusState
       , Internal_GenericHandlerState
       , Internal_GenericHandlerWithResponseState
       ) where

import Prelude

import Data.Either (hush)
import Data.Maybe (Maybe(..), fromMaybe, fromMaybe', isNothing)
import Data.Newtype (class Newtype, wrap)
import Effect (Effect)
import Erl.Atom (atom)
import Erl.Cowboy.Req (ReadBodyResult(..), Req, StatusCode(..), binding, method, readBody, replyWithoutBody)
import Erl.Data.Binary (Binary)
import Erl.Data.Binary.IOData (IOData, fromBinary, toBinary)
import Erl.Data.List (singleton, (:))
import Erl.Data.Map as Map
import Rtsv2.Handler.MimeType as MimeType
import Rtsv2.Utils (noprocToMaybe)
import Rtsv2.Web.Bindings as Bindings
import Shared.Stream (StreamId)
import Shared.Types (PoPName(..))
import Shared.Utils (lazyCrashIfMissing)
import Simple.JSON (class ReadForeign, class WriteForeign, writeJSON)
import Simple.JSON as JSON
import Stetson (HttpMethod(..), StetsonHandler)
import Stetson.Rest as Rest
import Unsafe.Coerce as Unsafe.Coerce




type GenericStetsonHandler a = StetsonHandler (Internal_GenericHandlerState a)
type Internal_GenericHandlerState a
  = { mPayload :: Maybe a
    }

genericPost :: forall a b. ReadForeign a => (a -> Effect b) -> GenericStetsonHandler a
genericPost proxiedFun =
  Rest.handler init
  # Rest.allowedMethods (Rest.result (POST : mempty))
  # Rest.malformedRequest malformedRequest
  # Rest.contentTypesAccepted (\req state -> Rest.result (singleton $ MimeType.json acceptJson) req state)
  # Rest.yeeha
  where
    init req =
      do
        (mPayload :: Maybe a) <- (hush <$> JSON.readJSON <$> binaryToString <$> allBody req mempty)
        Rest.initResult req { mPayload
                            }

    malformedRequest req state@{mPayload} =
      Rest.result (isNothing mPayload) req state

    acceptJson req state@{mPayload} = do
      let
        payload = fromMaybe' (lazyCrashIfMissing "impossible noEgestPayload") mPayload
      _ <- proxiedFun payload
      Rest.result true req state


type GenericStetsonHandlerWithResponse a b = StetsonHandler (Internal_GenericHandlerWithResponseState a b)
type Internal_GenericHandlerWithResponseState a b
  = { mPayload :: Maybe a
    , mResponse :: Maybe b
    }

genericPostWithResponse :: forall a b. ReadForeign a => WriteForeign b => (a -> Effect (Maybe b)) -> GenericStetsonHandlerWithResponse a b
genericPostWithResponse proxiedFun =
  Rest.handler init
  # Rest.allowedMethods (Rest.result (POST : mempty))
  # Rest.malformedRequest malformedRequest
  # Rest.contentTypesAccepted (\req state -> Rest.result (singleton $ MimeType.json acceptJson) req state)
  # Rest.contentTypesProvided (\req state -> Rest.result (singleton $ MimeType.json provideJson) req state)
  # Rest.yeeha
  where
    init req =
      do
        (mPayload :: Maybe a) <- (hush <$> JSON.readJSON <$> binaryToString <$> allBody req mempty)
        Rest.initResult req { mPayload
                            , mResponse : Nothing
                            }

    malformedRequest req state@{mPayload} =
      Rest.result (isNothing mPayload) req state

    acceptJson req state@{mPayload} = do
      let
        payload = fromMaybe' (lazyCrashIfMissing "impossible noEgestPayload") mPayload
      mResp <- proxiedFun payload
      Rest.result true req state{mResponse = mResp}

    provideJson req state@{mResponse} = do
      let
        response = fromMaybe "" $ JSON.writeJSON <$> mResponse
      Rest.result response req state




type GenericStetsonGetByStreamId a = StetsonHandler (Internal_GenericStatusState a)

type Internal_GenericStatusState a
  = { mData :: Maybe a
    }


genericGetByStreamId :: forall a. WriteForeign a =>  (StreamId -> Effect a) -> GenericStetsonGetByStreamId a
genericGetByStreamId = genericGetBy Bindings.streamIdBinding

genericGetByPoPName :: forall a. WriteForeign a =>  (PoPName -> Effect a) -> GenericStetsonGetByStreamId a
genericGetByPoPName = genericGetBy  Bindings.popNameBinding

genericGetBy :: forall a b. Newtype a String => WriteForeign b =>  String -> (a -> Effect b) -> GenericStetsonGetByStreamId b
genericGetBy bindElement getData =
  Rest.handler init
  # Rest.allowedMethods (Rest.result (GET : mempty))
  # Rest.contentTypesProvided (\req state -> Rest.result (singleton $ MimeType.json provideContent) req state)
  # Rest.yeeha
  where
    init req =
      let
        bindValue :: a
        bindValue = wrap $ fromMaybe' (lazyCrashIfMissing "stream_id binding missing") $ binding (atom bindElement) req
      in do
        mData <- noprocToMaybe $ getData bindValue
        Rest.initResult req
            { mData
            }

    provideContent req state@{mData} =
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
