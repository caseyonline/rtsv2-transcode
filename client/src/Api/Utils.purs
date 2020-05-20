module Rtsv2App.Api.Utils where

import Prelude

import Control.Monad.Reader (class MonadAsk, ask)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect.Aff as Aff
import Effect.Aff.Bus as Bus
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Effect.Exception (message)
import Effect.Ref as Ref
import Milkis as M
import Rtsv2App.Api.Request (OptionMethod, Token, fetchReq, printOriginUrl, printUrl, readToken, writeToken)
import Rtsv2App.Capability.LogMessages (class LogMessages, logError)
import Rtsv2App.Capability.Now (class Now)
import Rtsv2App.Data.Profile (Profile)
import Rtsv2App.Env (UrlEnv, UserEnv)
import Shared.Rtsv2.Router.Endpoint.Class (class RoutedEndpoint)
import Shared.Rtsv2.Types (ServerAddress)
import Unsafe.Coerce (unsafeCoerce)


-------------------------------------------------------------------------------
-- Request Helpers
-------------------------------------------------------------------------------
mkRequest
  :: forall m r e
   . MonadAff m
  => MonadAsk { urlEnv :: UrlEnv | r } m
  => RoutedEndpoint e
  => OptionMethod e
  -> m (Either String String)
mkRequest opts@{ endpoint, method } = do
  { urlEnv } <- ask
  response <- liftAff $ Aff.attempt $ fetchReq (M.URL $ printUrl urlEnv.curHostUrl endpoint) Nothing method
  case response of
    Left e    -> pure $ Left $ message e
    Right res -> handleResponse res

mkOriginRequest
  :: forall m r e
   . MonadAff m
  => MonadAsk { urlEnv :: UrlEnv | r } m
  => RoutedEndpoint e
  => ServerAddress
  -> OptionMethod e
  -> m (Either String String)
mkOriginRequest serverAdd opts@{ endpoint, method } = do
  response <- liftAff $ Aff.attempt $ fetchReq (M.URL $ printOriginUrl serverAdd endpoint) Nothing method
  case response of
    Left e    -> pure $ Left $ message e
    Right res -> handleResponse res

mkAuthRequest
  :: forall m r e
   . MonadAff m
  => MonadAsk { urlEnv :: UrlEnv | r } m
  => RoutedEndpoint e
  => OptionMethod e
  -> m (Either String String)
mkAuthRequest opts@{ endpoint, method } = do
  { urlEnv } <- ask
  token <- liftEffect readToken
  response <- liftAff $ Aff.attempt $ fetchReq (M.URL $ printUrl urlEnv.authUrl endpoint) token method
  case response of
    Left e    -> pure $ Left $ message e
    Right res -> handleResponse res

-------------------------------------------------------------------------------
-- Token Authentication
-------------------------------------------------------------------------------
authenticate
  :: forall m a r
   . MonadAff m
  => MonadAsk { urlEnv :: UrlEnv, userEnv :: UserEnv | r } m
  => LogMessages m
  => Now m
  => (UrlEnv -> a -> m (Either String (Tuple Token Profile)))
  -> a
  -> m (Maybe Profile)
authenticate req fields = do
  { userEnv, urlEnv } <- ask
  req urlEnv fields >>= case _ of
    Left err -> logError err *> pure Nothing
    Right (Tuple token profile) -> do
      liftEffect do
        writeToken token
        Ref.write (Just profile) userEnv.currentUser
      -- any time we write to the current user ref, we should also broadcast the change
      liftAff $ Bus.write (Just profile) userEnv.userBus
      pure (Just profile)


-------------------------------------------------------------------------------
-- Helper functions
-------------------------------------------------------------------------------
handleResponse
  :: forall m
  . MonadAff m
  => M.Response
  -> m (Either String String)
handleResponse response = do
  let code = M.statusCode response
      sText = statusText response
      url = M.url response
  case code of
    200 -> do
      res <- liftAff $ M.text response
      pure $ Right res

    _   -> pure $ Left $ (show code) <> ": " <> sText <> "\n Trying to access: " <> (show url)


statusText :: M.Response -> String
statusText response = response'.statusText
  where
    response' :: { statusText :: String }
    response' = unsafeCoerce response
