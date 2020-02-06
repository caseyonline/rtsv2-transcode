module Rtsv2App.Api.Request
  ( AuthFieldsRep(..)
  , BaseURL(..)
  , LoginFields(..)
  , OptionMethod(..)
  , ProfileJson(..)
  , ProfileTokenJson(..)
  , ProfileWithToken(..)
  , RegisterFields(..)
  , RequestMethod(..)
  , Token -- constructor and decoders not exported
  , Unlifted(..)
  , fetch
  , login
  , printBaseUrl
  , printFullUrl 
  , readToken
  , register
  , removeToken
  , requestUser
  , writeToken
    , withResponse
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, Error)
import Effect.Aff as Aff
import Effect.Aff.Class (class MonadAff, liftAff)
import Milkis as M
import Milkis.Impl.Window as MW
import Routing.Duplex (print)
import Rtsv2App.Api.Endpoint (Endpoint(..), endpointCodec)
import Rtsv2App.Data.Email (Email)
import Rtsv2App.Data.Profile (ProfileRep, Profile)
import Rtsv2App.Data.Username (Username)
import Simple.JSON (class ReadForeign, class WriteForeign)
import Simple.JSON as JSON
import Web.HTML (window)
import Web.HTML.Window (localStorage)
import Web.Storage.Storage (getItem, removeItem, setItem)

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------
newtype Token = Token String

derive instance eqToken :: Eq Token
derive instance ordToken :: Ord Token

derive newtype instance readForeignToken :: ReadForeign Token
derive newtype instance writeForeignToken :: WriteForeign Token

instance showToken :: Show Token where
  show (Token _) = "Token {- token -}"

-- | extend Profile type to have a token key
type ProfileWithToken = { | ProfileRep (token :: Token) }

type ProfileTokenJson =
  { user :: ProfileWithToken }

type ProfileJson =
  { user :: Profile }

data BaseURL =
    ApiUrl String
  | AuthURL String

data RequestMethod
  = Get
  | Post (Maybe String)
  | Put (Maybe String)
  | Delete

type OptionMethod =
  { endpoint :: Endpoint
  , method   :: RequestMethod
  }

type Unlifted a = a

type AuthFieldsRep box r = ( email :: Email, password :: box String | r )

type RegisterFields = { | AuthFieldsRep Unlifted (username :: Username) }

type LoginFields = { | AuthFieldsRep Unlifted () }


-------------------------------------------------------------------------------
-- Default RequestOptions
-------------------------------------------------------------------------------
fetch :: M.URL -> Maybe Token -> OptionMethod -> Aff M.Response
fetch url token { method } = case method of
  Get    -> f url
            { method: M.getMethod
            , headers: getHeader token
            }

  Post b -> f url
            { method: M.postMethod
            , headers: getHeader token
            , body: fromMaybe "" b
            }

  Put b  -> f url
            { method: M.postMethod
            , headers: getHeader token
            , body: fromMaybe "" b
            }

  Delete -> f url { method: M.deleteMethod
            , headers: getHeader token
            }
  where
    f :: M.Fetch
    f = M.fetch MW.windowFetch

getHeader :: Maybe Token -> M.Headers
getHeader = case _ of
  Nothing -> M.makeHeaders { "Content-Type": "application/json" }
  Just (Token t) ->  M.makeHeaders { "Authorization": "Token " <> t }

-------------------------------------------------------------------------------
-- User Request functions
-------------------------------------------------------------------------------
login :: forall m. MonadAff m => BaseURL -> LoginFields -> m (Either String (Tuple Token Profile))
login baseUrl fields =
  let method = Post (Just (JSON.writeJSON { user: fields }))
   in requestUser baseUrl { endpoint: Login, method }

register :: forall m. MonadAff m => BaseURL -> RegisterFields -> m (Either String (Tuple Token Profile))
register baseUrl fields =
  let method = Post (Just (JSON.writeJSON { user: fields }))
   in requestUser baseUrl { endpoint: Users, method }

requestUser :: forall m. MonadAff m => BaseURL -> OptionMethod -> m (Either String (Tuple Token Profile))
requestUser baseUrl opts@{ endpoint } = do
  response <- liftAff $ Aff.attempt $ fetch (M.URL $ printFullUrl baseUrl endpoint) Nothing opts
  withResponse response \(result :: ProfileTokenJson) -> do
                            let u = result.user
                            (Tuple (u.token) { bio: u.bio
                                             , image: u.image
                                             , username: u.username
                                             })

withResponse
  :: forall a b m
  .  MonadAff m
  => JSON.ReadForeign a
  => Either Error (M.Response)
  -> (a -> b)
  -> m (Either String b)
withResponse response action =
  case response of
    Left e    -> pure $ Left $ show e
    Right res -> do
      r <- liftAff $ M.text res
      withDecoded r action

withDecoded :: forall a b m. MonadAff m => JSON.ReadForeign a => String -> (a -> b) -> m (Either String b)
withDecoded json action = case JSON.readJSON json of
  Left err -> pure $ Left $ show err
  Right v -> pure $ Right $ action v

-------------------------------------------------------------------------------
-- Print URL helpers
-------------------------------------------------------------------------------
printBaseUrl :: BaseURL -> String
printBaseUrl url = case url of
  ApiUrl a -> a
  AuthURL a -> a

printFullUrl :: BaseURL -> Endpoint -> String
printFullUrl baseUrl endpoint = printBaseUrl baseUrl <> (print endpointCodec endpoint)

-------------------------------------------------------------------------------
-- LocalStorage Token actions
-------------------------------------------------------------------------------
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
