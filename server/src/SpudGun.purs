module SpudGun
( post
, post'
, put'
, getText
, getJson
, get'
, delete
, delete'
, bodyToJSON
, bodyToString
, ParseError
, SpudResult
, SpudResponse(..)
, SpudError(..)
, StatusCode
, Headers
, Body
) where

import Prelude

import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Newtype (class Newtype, unwrap, wrap)
import Effect (Effect)
import Erl.Data.List (List, nil, (:))
import Erl.Data.Tuple (Tuple2, tuple2)
import Foreign (Foreign, MultipleErrors)
import Simple.JSON (class ReadForeign, class WriteForeign)
import Simple.JSON as JSON

newtype StatusCode = StatusCode Int

type Headers = List (Tuple2 String String)

newtype Body = Body String

data SpudError = RequestError Foreign
               | ResponseError SpudResponse

data SpudResponse = SpudResponse StatusCode Headers Body

type SpudResult = Either SpudError SpudResponse

data JsonResponseError = SpudError SpudError
                       | JsonError MultipleErrors

bodyToString :: SpudResult -> Either SpudError String
bodyToString = map (\(SpudResponse _ _ b) -> unwrap b)

bodyToJSON :: forall a. ReadForeign a => SpudResult -> Either JsonResponseError a
bodyToJSON result =
  (lmap JsonError <<< JSON.readJSON) =<< (toBody <$> (lmap SpudError result))
  where
    toBody (SpudResponse _ _ b) = unwrap b

get' :: String -> Headers -> Effect SpudResult
get' = getImpl requestError responseError responseSuccess

getText :: String -> Effect SpudResult
getText s = get' s (tuple2 "Accept" "text/plain" : nil)

getJson :: String -> Effect SpudResult
getJson s = get' s (tuple2 "Accept" "application/json" : nil)

post :: String -> String -> Effect SpudResult
post s b = post' s b (tuple2 "Content-Type" "application/json" : tuple2 "Accept" "application/json" : nil)

post' :: String -> String -> Headers -> Effect SpudResult
post' = postImpl requestError responseError responseSuccess

put' :: String -> String -> Headers -> Effect SpudResult
put' = putImpl requestError responseError responseSuccess

delete :: String -> Effect SpudResult
delete s = delete' s (tuple2 "Content-Type" "application/json" : tuple2 "Accept" "application/json" : nil)

delete' :: String -> Headers -> Effect SpudResult
delete' = deleteImpl requestError responseError responseSuccess

type RequestErrorFun = Foreign -> SpudResult
type ResponseErrorFun = Int -> Headers -> String -> SpudResult
type ResponseSuccessFun = Int -> Headers -> String -> SpudResult

foreign import getImpl :: RequestErrorFun -> ResponseErrorFun -> ResponseSuccessFun -> String -> Headers -> Effect SpudResult

foreign import putImpl :: RequestErrorFun -> ResponseErrorFun -> ResponseSuccessFun -> String -> String -> Headers -> Effect SpudResult

foreign import postImpl :: RequestErrorFun -> ResponseErrorFun -> ResponseSuccessFun -> String -> String -> Headers -> Effect SpudResult

foreign import deleteImpl :: RequestErrorFun -> ResponseErrorFun -> ResponseSuccessFun -> String -> Headers -> Effect SpudResult

requestError :: RequestErrorFun
requestError f = Left $ RequestError f

responseError :: ResponseErrorFun
responseError s h b = Left $ ResponseError (SpudResponse (wrap s) h (wrap b))

responseSuccess :: Int -> Headers -> String -> SpudResult --ResponseSuccessFun
responseSuccess s h b = Right $ SpudResponse (wrap s) h (wrap b)

derive instance newtypeStatusCode :: Newtype StatusCode _
derive newtype instance eqStatusCode :: Eq StatusCode
derive newtype instance ordStatusCode :: Ord StatusCode
derive newtype instance showStatusCode :: Show StatusCode
derive newtype instance readForeignStatusCode :: ReadForeign StatusCode
derive newtype instance writeForeignStatusCode :: WriteForeign StatusCode

derive instance newtypeBody :: Newtype Body _
derive newtype instance eqBody :: Eq Body
derive newtype instance ordBody :: Ord Body
derive newtype instance showBody :: Show Body
derive newtype instance readForeignBody :: ReadForeign Body
derive newtype instance writeForeignBody :: WriteForeign Body
