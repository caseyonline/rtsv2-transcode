module Rtsv2App.Api.Request
  ( AuthFieldsRep(..)
  , LoginFields(..)
  , OptionMethod(..)
  , ProfileJson(..)
  , ProfileTokenJson(..)
  , ProfileWithToken(..)
  , RegisterFields(..)
  , RequestMethod(..)
  , Token -- constructor and decoders not exported
  , Unlifted(..)
  , fetchReq
  , login
  , printUrl
  , printOriginUrl
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
import Data.Newtype (class Newtype, unwrap)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, Error)
import Effect.Aff as Aff
import Effect.Aff.Class (class MonadAff, liftAff)
import Milkis as M
import Milkis.Impl.Window as MW
import Routing.Duplex (print)
import Rtsv2App.Api.Endpoint.Auth as Auth
import Rtsv2App.Data.Email (Email)
import Rtsv2App.Data.Profile (ProfileRep, Profile)
import Rtsv2App.Data.Username (Username)
import Rtsv2App.Env (UrlEnv)
import Shared.Rtsv2.Router.Endpoint.Class (class RoutedEndpoint, getRoute)
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

data RequestMethod
  = Get
  | Post (Maybe String)
  | Put (Maybe String)
  | Delete

type OptionMethod endpoint =
  { endpoint :: endpoint
  , method   :: RequestMethod
  }

type Unlifted a = a

type AuthFieldsRep box r = ( email :: Email, password :: box String | r )

type RegisterFields = { | AuthFieldsRep Unlifted (username :: Username) }

type LoginFields = { | AuthFieldsRep Unlifted () }


-------------------------------------------------------------------------------
-- Default RequestOptions
-------------------------------------------------------------------------------
fetchReq :: M.URL -> Maybe Token -> RequestMethod -> Aff M.Response
fetchReq url token method = case method of
  Get    -> f url
            { method: M.getMethod
            , headers: getHeader token
            }

  Post b -> f url
            { method: M.postMethod
            , headers: setHeader token
            , body: fromMaybe "" b
            }

  Put b  -> f url
            { method: M.postMethod
            , headers: setHeader token
            , body: fromMaybe "" b
            }

  Delete -> f url { method: M.deleteMethod
            , headers: setHeader token
            }
  where
    f :: M.Fetch
    f = M.fetch MW.windowFetch

getHeader :: Maybe Token -> M.Headers
getHeader = case _ of
  Nothing -> M.makeHeaders {}
  Just (Token t) ->  M.makeHeaders { "Authorization": "Token " <> t }

setHeader :: Maybe Token -> M.Headers
setHeader = case _ of
  Nothing -> M.makeHeaders { "Content-Type": "application/json" }
  Just (Token t) ->  M.makeHeaders { "Authorization": "Token " <> t }

-------------------------------------------------------------------------------
-- User Request functions
-------------------------------------------------------------------------------
requestUser :: forall m e. MonadAff m => RoutedEndpoint e => UrlEnv -> OptionMethod e -> m (Either String (Tuple Token Profile))
requestUser url opts@{ endpoint, method } = do
  response <- liftAff $ Aff.attempt $ fetchReq (M.URL $ printUrl url.authUrl endpoint) Nothing method
  withResponse response \(result :: ProfileTokenJson) -> do
                            let u = result.user
                            (Tuple (u.token) { bio: u.bio
                                             , image: u.image
                                             , username: u.username
                                             })

login :: forall m. MonadAff m => UrlEnv -> LoginFields -> m (Either String (Tuple Token Profile))
login urlEnv fields =
  let method = Post (Just (JSON.writeJSON { user: fields }))
   in requestUser urlEnv { endpoint: Auth.LoginE, method }

register :: forall m. MonadAff m => UrlEnv -> RegisterFields -> m (Either String (Tuple Token Profile))
register urlEnv fields =
  let method = Post (Just (JSON.writeJSON { user: fields }))
   in requestUser urlEnv { endpoint: Auth.UsersE, method }

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
-- Print URL helper
-------------------------------------------------------------------------------
printUrl :: forall a b. Newtype a String => RoutedEndpoint b => a -> b -> String
printUrl url endP = unwrap url <> print (getRoute endP) endP

printOriginUrl  :: forall a b. Newtype a String => RoutedEndpoint b => a -> b -> String
printOriginUrl url endP = "http://" <> unwrap url <> ":3002" <> print (getRoute endP) endP

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
