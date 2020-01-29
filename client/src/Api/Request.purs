module Rtsv2App.Api.Request
  ( Token -- constructor and decoders not exported
  , BaseURL(..)
  , RequestMethod(..)
  , RequestOptions(..)
  , defaultRequest
  , Unlifted(..)
  , RegisterFields(..)
  , LoginFields(..)
  , AuthFieldsRep(..)
  , login
  , register
  , readToken
  , writeToken
  , removeToken
  ) where

import Prelude

import Affjax (Request, printResponseFormatError, request)
import Affjax.RequestBody as RB
import Affjax.RequestHeader (RequestHeader(..))
import Affjax.ResponseFormat as RF
import Rtsv2App.Api.Endpoint (Endpoint(..), endpointCodec)
import Rtsv2App.Data.Email (Email)
import Rtsv2App.Data.Profile (Profile)
import Rtsv2App.Data.Username (Username)
import Rtsv2App.Data.Utils (decodeAt)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode.Struct.Tolerant as Tolerant
import Data.Argonaut.Encode (encodeJson)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff.Class (class MonadAff, liftAff)
import Routing.Duplex (print)
import Web.HTML (window)
import Web.HTML.Window (localStorage)
import Web.Storage.Storage (getItem, removeItem, setItem)

newtype Token = Token String

derive instance eqToken :: Eq Token
derive instance ordToken :: Ord Token

instance showToken :: Show Token where
  show (Token _) = "Token {- token -}"

data BaseURL =
    ApiUrl String
  | AuthURL String

data RequestMethod
  = Get
  | Post (Maybe Json)
  | Put (Maybe Json)
  | Delete

type RequestOptions =
  { endpoint :: Endpoint
  , method :: RequestMethod
  }

defaultRequest :: BaseURL -> Maybe Token -> RequestOptions -> Request Json
defaultRequest baseUrl auth { endpoint, method } =
  { method: Left method 
  , url: (printUrl baseUrl) <> print endpointCodec endpoint
  , headers: case auth of
      Nothing -> []
      Just (Token t) -> [ RequestHeader "Authorization" $ "Token " <> t ]
  , content: RB.json <$> body
  , username: Nothing
  , password: Nothing
  , withCredentials: false
  , responseFormat: RF.json
  }
  where
  Tuple method body = case method of
    Get -> Tuple GET Nothing
    Post b -> Tuple POST b
    Put b -> Tuple PUT b
    Delete -> Tuple DELETE Nothing

type Unlifted a = a

type AuthFieldsRep box r = ( email :: Email, password :: box String | r )

type RegisterFields = { | AuthFieldsRep Unlifted (username :: Username) }

type LoginFields = { | AuthFieldsRep Unlifted () }

printUrl :: BaseURL -> String
printUrl url = case url of
  ApiUrl a -> a
  AuthURL a -> a

login :: forall m. MonadAff m => BaseURL -> LoginFields -> m (Either String (Tuple Token Profile))
login baseUrl fields = 
  let method = Post $ Just $ encodeJson { user: fields } 
   in requestUser baseUrl { endpoint: Login, method }

register :: forall m. MonadAff m => BaseURL -> RegisterFields -> m (Either String (Tuple Token Profile))
register baseUrl fields = 
  let method = Post $ Just $ encodeJson { user: fields }
   in requestUser baseUrl { endpoint: Users, method } 

requestUser :: forall m. MonadAff m => BaseURL -> RequestOptions -> m (Either String (Tuple Token Profile))
requestUser baseUrl opts = do
  res <- liftAff $ request $ defaultRequest baseUrl Nothing opts
  pure $ decodeAuthProfile =<< decodeAt "user" =<< lmap printResponseFormatError res.body

decodeAuthProfile :: Json -> Either String (Tuple Token Profile)
decodeAuthProfile =
  decodeParts
    (map Token <<< decodeAt "token")
    Tolerant.decodeJson

decodeParts :: forall a b c f. Apply f => (a -> f b) -> (a -> f c) -> a -> f (Tuple b c)
decodeParts decoder1 decoder2 value =
  Tuple <$> decoder1 value <*> decoder2 value

tokenKey = "token" :: String

readToken :: Effect (Maybe Token)
readToken = do
  str <- getItem tokenKey =<< localStorage =<< window
  pure $ map Token str

writeToken :: Token -> Effect Unit
writeToken (Token str) =
  setItem tokenKey str =<< localStorage =<< window

removeToken :: Effect Unit
removeToken =
  removeItem tokenKey =<< localStorage =<< window
