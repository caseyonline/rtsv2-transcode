module Rtsv2App.Api.Utils where

import Prelude

import Control.Monad.Reader (class MonadAsk, ask)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Debug.Trace (spy)
import Effect.Aff as Aff
import Effect.Aff.Bus as Bus
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Milkis as M
import Rtsv2App.Api.Request (BaseURL, OptionMethod, Token, fetch, printBaseUrl, printFullUrl, readToken, writeToken)
import Rtsv2App.Capability.LogMessages (class LogMessages, logError)
import Rtsv2App.Capability.Now (class Now)
import Rtsv2App.Data.Profile (Profile)
import Rtsv2App.Env (UserEnv)

-------------------------------------------------------------------------------
-- Request Helpers
-------------------------------------------------------------------------------
mkRequest
  :: forall m r
   . MonadAff m
  => MonadAsk { apiUrl :: BaseURL | r } m
  => OptionMethod
  -> m String
mkRequest opts@{ endpoint } = do
  { apiUrl } <- ask
  response <- liftAff $ Aff.attempt $ fetch (M.URL $ spy "API URL" $ printFullUrl apiUrl endpoint) Nothing opts
  case response of
    Left e    -> pure $ "Error making request: " <> show e
    Right res -> do
      pure =<< liftAff $ M.text res

mkAuthRequest
  :: forall m r
   . MonadAff m
  => MonadAsk { authUrl :: BaseURL | r } m
  => OptionMethod
  -> m String
mkAuthRequest opts@{ endpoint } = do
  { authUrl } <- ask
  token <- liftEffect readToken
  response <- liftAff $ Aff.attempt $ fetch (M.URL $ printFullUrl authUrl endpoint) token opts
  case response of
    Left e    -> pure $ "Error making request: " <> show e
    Right res -> do
      pure =<< liftAff $ M.text res


-------------------------------------------------------------------------------
-- Token Authentication
-------------------------------------------------------------------------------
authenticate
  :: forall m a r
   . MonadAff m
  => MonadAsk { authUrl :: BaseURL, userEnv :: UserEnv | r } m
  => LogMessages m
  => Now m
  => (BaseURL -> a -> m (Either String (Tuple Token Profile))) 
  -> a 
  -> m (Maybe Profile)
authenticate req fields = do 
  { authUrl, userEnv } <- ask
  req authUrl fields >>= case _ of
    Left err -> logError err *> pure Nothing
    Right (Tuple token profile) -> do 
      liftEffect do 
        writeToken token 
        Ref.write (Just profile) userEnv.currentUser
      -- any time we write to the current user ref, we should also broadcast the change 
      liftAff $ Bus.write (Just profile) userEnv.userBus
      pure (Just profile)
