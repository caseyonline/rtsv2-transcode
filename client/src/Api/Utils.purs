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
import Rtsv2App.Api.Request (OptionMethod, Token, fetchReq, printOriginUrl, printUrl, readToken, writeToken)
import Rtsv2App.Capability.LogMessages (class LogMessages, logError)
import Rtsv2App.Capability.Now (class Now)
import Rtsv2App.Data.Profile (Profile)
import Rtsv2App.Env (UrlEnv, UserEnv)
import Shared.Types (ServerAddress)


-------------------------------------------------------------------------------
-- Request Helpers
-------------------------------------------------------------------------------
mkRequest
  :: forall m r
   . MonadAff m
  => MonadAsk { urlEnv :: UrlEnv | r } m
  => OptionMethod
  -> m String
mkRequest opts@{ endpoint, method } = do
  { urlEnv } <- ask
  response <- liftAff $ Aff.attempt $ fetchReq (M.URL $ printUrl urlEnv.curHostUrl endpoint) Nothing method
  case response of
    Left e    -> pure $ "Error making request: " <> show e
    Right res -> do
      pure =<< liftAff $ M.text res

mkOriginRequest
  :: forall m r
   . MonadAff m
  => MonadAsk { urlEnv :: UrlEnv | r } m
  => ServerAddress
  -> OptionMethod
  -> m String
mkOriginRequest serverAdd opts@{ endpoint, method } = do
  response <- liftAff $ Aff.attempt $ fetchReq (M.URL $ printOriginUrl serverAdd endpoint) Nothing method
  case response of
    Left e    -> pure $ "Error making request: " <> show e
    Right res -> do
      pure =<< liftAff $ M.text res

mkAuthRequest
  :: forall m r
   . MonadAff m
  => MonadAsk { urlEnv :: UrlEnv | r } m
  => OptionMethod
  -> m String
mkAuthRequest opts@{ endpoint, method } = do
  { urlEnv } <- ask
  token <- liftEffect readToken
  response <- liftAff $ Aff.attempt $ fetchReq (M.URL $ printUrl urlEnv.authUrl endpoint) token method
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
