module StetsonHelper
       ( genericPost
       , genericPostWithResponse
       , genericGetBySlotId
       , genericGetBySlotIdAndRole
       , genericGetByPoPName
       , genericGetBy
       , genericGetBy2
       , genericGetByInt
       , genericGetByInt2
       , genericGet
       , genericGet2
       , genericGetText
       , genericProxyBySlotId
       , genericProvideJson
       , GenericStetsonGet
       , GenericStetsonGet2
       , GenericStetsonGetBySlotId
       , GenericStetsonHandler
       , GenericStetsonHandlerWithResponse

       , allBody
       , binaryToString

       , preHookSpyReq
       , preHookSpyReqState
       , preHookSpyState

       , GenericHandlerState
       , GenericStatusState(..)
       , GenericStatusState2
       , GenericHandlerWithResponseState
       , GenericProxyState(..)
       ) where

import Prelude

import Data.Either (hush)
import Data.Int (fromString)
import Data.Maybe (Maybe(..), fromMaybe, fromMaybe', isJust, isNothing)
import Data.Newtype (class Newtype, unwrap, wrap)
import Effect (Effect)
import Erl.Atom (atom)
import Erl.Cowboy.Handlers.Rest (moved, notMoved)
import Erl.Cowboy.Req (ReadBodyResult(..), Req, StatusCode(..), binding, readBody, replyWithoutBody)
import Erl.Data.Binary (Binary)
import Erl.Data.Binary.IOData (IOData, fromBinary, toBinary)
import Erl.Data.List (List, singleton, (:))
import Erl.Data.Map as Map
import Erl.Data.Tuple (Tuple2, fst, snd, tuple2)
import Logger (spy)
import Rtsv2.Agents.Locator.Types (LocalOrRemote, fromLocalOrRemote)
import Rtsv2.Handler.MimeType as MimeType
import Rtsv2.Router.Endpoint (Endpoint, makeUrl)
import Rtsv2.Utils (noprocToMaybe)
import Rtsv2.Web.Bindings as Bindings
import Shared.Stream (SlotId, SlotRole)
import Shared.Types (PoPName, Server)
import Shared.Utils (lazyCrashIfMissing)
import Simple.JSON (class ReadForeign, class WriteForeign, writeJSON)
import Simple.JSON as JSON
import Stetson (HttpMethod(..), RestResult, StetsonHandler)
import Stetson.Rest as Rest
import Unsafe.Coerce as Unsafe.Coerce

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



type GenericStetsonGet a = StetsonHandler (GenericStatusState a)
type GenericStetsonGetBySlotId a = StetsonHandler (GenericStatusState a)

newtype GenericStatusState a = GenericStatusState { mData :: Maybe a }



genericGetBySlotIdAndRole :: forall a. WriteForeign a =>  (SlotId -> SlotRole -> Effect a) -> GenericStetsonGet a
genericGetBySlotIdAndRole getFun =  Rest.handler init
  # Rest.allowedMethods (Rest.result (GET : mempty))
  # Rest.contentTypesProvided (\req state -> Rest.result (singleton $ MimeType.json genericProvideJson) req state)
  # Rest.yeeha
  where
    init req =
      let
        slotId = Bindings.slotId req
        streamRole = Bindings.streamRole req
      in do
        mData <- noprocToMaybe $ getFun slotId streamRole
        Rest.initResult req $ GenericStatusState { mData }




genericGetBySlotId :: forall a. WriteForeign a =>  (SlotId -> Effect a) -> GenericStetsonGetBySlotId a
genericGetBySlotId = genericGetByInt Bindings.slotIdBindingLiteral

genericGetByPoPName :: forall a. WriteForeign a =>  (PoPName -> Effect a) -> GenericStetsonGetBySlotId a
genericGetByPoPName = genericGetBy  Bindings.popNameBindingLiteral

genericGet :: forall a. WriteForeign a => (Unit -> Effect a) -> GenericStetsonGet a
genericGet getData =
  Rest.handler init
  # Rest.allowedMethods (Rest.result (GET : mempty))
  # Rest.contentTypesProvided (\req state -> Rest.result (singleton $ MimeType.json genericProvideJson) req state)
  # Rest.yeeha
  where
    init req = do
      mData <- noprocToMaybe $ getData unit
      Rest.initResult req $ GenericStatusState { mData }

genericGetText :: (Unit -> Effect String) -> GenericStetsonGet String
genericGetText getData =
  Rest.handler init
  # Rest.allowedMethods (Rest.result (GET : mempty))
  # Rest.contentTypesProvided (\req state -> Rest.result (singleton $ MimeType.text genericProvideText) req state)
  # Rest.yeeha
  where
    init req = do
      mData <- noprocToMaybe $ getData unit
      Rest.initResult req $ GenericStatusState { mData }




type GenericStetsonGet2 = StetsonHandler GenericStatusState2

newtype GenericStatusState2 = GenericStatusState2 { bindValues :: List String }

genericGet2 :: List String -> List (Tuple2 String (List String -> Effect String)) -> GenericStetsonGet2
genericGet2 bindings getDatas =
  Rest.handler init
  # Rest.allowedMethods (Rest.result (GET : mempty))
  # Rest.contentTypesProvided (\req state -> Rest.result ((\tuple -> tuple2 (fst tuple) (provideContent (snd tuple))) <$> getDatas) req state)
  # Rest.yeeha
  where
    init req =
      let
        bindValues :: List String
        bindValues = (\bindElement -> fromMaybe' (lazyCrashIfMissing (bindElement <> " binding missing")) $ binding (atom bindElement) req) <$> bindings
      in
       Rest.initResult req $ GenericStatusState2 { bindValues }

    provideContent getData req state@(GenericStatusState2 {bindValues}) = do
      mData <- noprocToMaybe $ getData bindValues
      case mData of
        Nothing ->
          do
            newReq <- replyWithoutBody (StatusCode 404) Map.empty req
            Rest.stop newReq state
        Just theData ->
          Rest.result theData req state

genericGetBy :: forall a b. Newtype a String => WriteForeign b =>  String -> (a -> Effect b) -> GenericStetsonGetBySlotId b
genericGetBy bindElement getData =
  Rest.handler init
  # Rest.allowedMethods (Rest.result (GET : mempty))
  # Rest.contentTypesProvided (\req state -> Rest.result (singleton $ MimeType.json genericProvideJson) req state)
  # Rest.yeeha
  where
    init req =
      let
        bindValue :: a
        bindValue = wrap $ fromMaybe' (lazyCrashIfMissing (bindElement <> " binding missing")) $ binding (atom bindElement) req
      in do
        mData <- noprocToMaybe $ getData bindValue
        Rest.initResult req $ GenericStatusState { mData }

genericGetByInt :: forall a b. Newtype a Int => WriteForeign b =>  String -> (a -> Effect b) -> GenericStetsonGetBySlotId b
genericGetByInt bindElement getData =
  Rest.handler init
  # Rest.allowedMethods (Rest.result (GET : mempty))
  # Rest.contentTypesProvided (\req state -> Rest.result (singleton $ MimeType.json genericProvideJson) req state)
  # Rest.yeeha
  where
    init req =
      let
        bindValue :: a
        bindValue = wrap $ fromMaybe' (lazyCrashIfMissing (bindElement <> " binding missing")) $ (fromString =<< binding (atom bindElement) req)
      in do
        mData <- noprocToMaybe $ getData bindValue
        Rest.initResult req $ GenericStatusState { mData }


genericGetBy2 :: forall a b c. Newtype a String => Newtype b String => WriteForeign c =>  String -> String -> (a -> b -> Effect c) -> GenericStetsonGet c
genericGetBy2 bindElement1 bindElement2 getData =
  Rest.handler init
  # Rest.allowedMethods (Rest.result (GET : mempty))
  # Rest.contentTypesProvided (\req state -> Rest.result (singleton $ MimeType.json genericProvideJson) req state)
  # Rest.yeeha
  where
    init req =
      let
        bindValue1 :: a
        bindValue1 = wrap $ fromMaybe' (lazyCrashIfMissing (bindElement1 <> " binding missing")) $ binding (atom bindElement1) req
        bindValue2 :: b
        bindValue2 = wrap $ fromMaybe' (lazyCrashIfMissing (bindElement2 <> " binding missing")) $ binding (atom bindElement2) req
      in do
        mData <- noprocToMaybe $ getData bindValue1 bindValue2
        Rest.initResult req $ GenericStatusState { mData }

genericGetByInt2 :: forall a b c. Newtype a Int => Newtype b String => WriteForeign c =>  String -> String -> (a -> b -> Effect c) -> GenericStetsonGet c
genericGetByInt2 bindElement1 bindElement2 getData =
  Rest.handler init
  # Rest.allowedMethods (Rest.result (GET : mempty))
  # Rest.contentTypesProvided (\req state -> Rest.result (singleton $ MimeType.json genericProvideJson) req state)
  # Rest.yeeha
  where
    init req =
      let
        bindValue1 :: a
        bindValue1 = wrap $ fromMaybe' (lazyCrashIfMissing (bindElement1 <> " binding missing")) $ (fromString =<< binding (atom bindElement1) req)
        bindValue2 :: b
        bindValue2 = wrap $ fromMaybe' (lazyCrashIfMissing (bindElement2 <> " binding missing")) $ binding (atom bindElement2) req
      in do
        mData <- noprocToMaybe $ getData bindValue1 bindValue2
        Rest.initResult req $ GenericStatusState { mData }


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



newtype GenericProxyState
  = GenericProxyState { whereIsResp :: Maybe Server
                      , slotId :: SlotId
                      }





genericProxyBySlotId :: (SlotId -> Effect (Maybe (LocalOrRemote Server))) -> (SlotId -> Endpoint) -> StetsonHandler GenericProxyState
genericProxyBySlotId whereIsFun endpointFun =
  Rest.handler init
  # Rest.allowedMethods (Rest.result (GET : mempty))
  # Rest.resourceExists resourceExists
  # Rest.previouslyExisted previouslyExisted
  # Rest.movedTemporarily movedTemporarily

  # Rest.yeeha
  where
    init req =
      let
        bindElement = Bindings.slotIdBindingLiteral
        bindValue :: SlotId
        bindValue = wrap $ fromMaybe' (lazyCrashIfMissing (bindElement <> " binding missing")) $ binding (atom bindElement) req
      in do
        whereIsResp <- (map fromLocalOrRemote) <$> whereIsFun bindValue
        Rest.initResult req $
            GenericProxyState { whereIsResp
                              , slotId : bindValue
                              }

    resourceExists req state =
      Rest.result false req state

    previouslyExisted req state@(GenericProxyState {whereIsResp}) =
      Rest.result (isJust whereIsResp) req state

    movedTemporarily req state@(GenericProxyState {whereIsResp, slotId}) =
      case whereIsResp of
        Just server ->
          let
            url = makeUrl server (endpointFun slotId)
          in
            Rest.result (moved $ unwrap url) req state
        _ ->
          Rest.result notMoved req state



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
