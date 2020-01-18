module SpudGun
       ( postJson
       , get
       , getText
       , getJson
       , delete
       , bodyToJSON
       , bodyToString
       , JsonResponseError
       , SpudResult
       , SpudResponse(..)
       , SpudError(..)
       , StatusCode
       , Headers
       , Body
       , module Erl
       ) where

import Prelude

import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Newtype (class Newtype, unwrap, wrap)
import Effect (Effect)
import Erl.Atom (Atom, atom)
import Erl.Data.List (List, nil, (:))
import Erl.Data.Tuple (Tuple2, tuple2)
import Erl.Utils (Milliseconds, Url)
import Erl.Utils (Url, Milliseconds) as Erl
import Foreign (Foreign, MultipleErrors)
import Prim.Row (class Union)
import Simple.JSON (class ReadForeign, class WriteForeign)
import Simple.JSON as JSON


--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------
newtype StatusCode = StatusCode Int
derive instance newtypeStatusCode :: Newtype StatusCode _
derive newtype instance eqStatusCode :: Eq StatusCode
derive newtype instance ordStatusCode :: Ord StatusCode
derive newtype instance showStatusCode :: Show StatusCode
derive newtype instance readForeignStatusCode :: ReadForeign StatusCode
derive newtype instance writeForeignStatusCode :: WriteForeign StatusCode

newtype Body = Body String
derive instance newtypeBody :: Newtype Body _
derive newtype instance eqBody :: Eq Body
derive newtype instance ordBody :: Ord Body
derive newtype instance showBody :: Show Body
derive newtype instance readForeignBody :: ReadForeign Body
derive newtype instance writeForeignBody :: WriteForeign Body

type Headers = List (Tuple2 String String)

data SpudError = RequestError Foreign
               | ResponseError SpudResponse

data SpudResponse = SpudResponse StatusCode Headers Body

type SpudResult = Either SpudError SpudResponse

data JsonResponseError = SpudError SpudError
                       | JsonError MultipleErrors

type RequestErrorFun = Foreign -> SpudResult
type ResponseErrorFun = Int -> Headers -> String -> SpudResult
type ResponseSuccessFun = Int -> Headers -> String -> SpudResult


--------------------------------------------------------------------------------
-- Type constraints as inspired by https://github.com/justinwoo/purescript-milkis
--------------------------------------------------------------------------------
type RequestOptionsWithBody =
  ( body :: String
  | RequestOptions
  )

type RequestOptions =
  ( headers :: Headers
  , followRedirect :: Boolean
  , connectTimeoutMs :: Int
  , requestTimeoutMs :: Milliseconds
  , bodyTimeoutMs    :: Int
  )

type HttpRequest
   = forall options trash
   . Union options trash RequestOptions
  => Url
  -> Record (options)
  -> Effect SpudResult

type HttpRequestWithBody
   = forall options trash
   . Union options trash RequestOptionsWithBody
  => Url
  -> Record (options)
  -> Effect SpudResult

--------------------------------------------------------------------------------
-- Basic HTTP methods
--------------------------------------------------------------------------------
get :: HttpRequest
get = makeRequest $ atom "get"

delete :: HttpRequest
delete = makeRequest $ atom "delete"

post :: HttpRequestWithBody
post = makeRequest $ atom "post"

put :: HttpRequestWithBody
put = makeRequest $ atom "put"

--------------------------------------------------------------------------------
-- Commonly used specifics
--------------------------------------------------------------------------------
getText :: Url -> Effect SpudResult
getText url = get url {headers : tuple2 "accept" "text/plain" : nil}

getJson :: Url -> Effect SpudResult
getJson url = get url {headers : tuple2 "Accept" "application/json" : nil}

postJson :: Url -> String -> Effect SpudResult
postJson url body = post url {body, headers: tuple2 "Content-Type" "application/json" : tuple2 "Accept" "application/json" : nil}

--------------------------------------------------------------------------------
-- Body conversion helpers
--------------------------------------------------------------------------------
bodyToString :: SpudResult -> Either SpudError String
bodyToString = map (\(SpudResponse _ _ b) -> unwrap b)

bodyToJSON :: forall a. ReadForeign a => SpudResult -> Either JsonResponseError a
bodyToJSON result =
  (lmap JsonError <<< JSON.readJSON) =<< (toBody <$> (lmap SpudError result))
  where
    toBody (SpudResponse _ _ b) = unwrap b

--------------------------------------------------------------------------------
-- Internal functions
--------------------------------------------------------------------------------
makeRequest :: forall options.  Atom -> Url -> Record(options) -> Effect SpudResult
makeRequest method url options = makeRequestImpl requestError responseError responseSuccess method (unwrap url) options

-- TODO - we get a runtime crash if you put the fancy Union constraint on the foreign import...
foreign import makeRequestImpl ::
  forall options.
  RequestErrorFun -> ResponseErrorFun -> ResponseSuccessFun -> Atom -> String -> Record(options) -> Effect SpudResult

requestError :: RequestErrorFun
requestError = Left <<< RequestError

responseError :: ResponseErrorFun
responseError s h b = Left $ ResponseError (SpudResponse (wrap s) h (wrap b))

responseSuccess :: Int -> Headers -> String -> SpudResult --ResponseSuccessFun
responseSuccess s h b = Right $ SpudResponse (wrap s) h (wrap b)
