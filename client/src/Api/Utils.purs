module Rtsv2App.Api.Utils where

import Prelude

import Affjax (request)
import Rtsv2App.Api.Request (BaseURL, RequestOptions, Token, defaultRequest, readToken, writeToken)
import Rtsv2App.Capability.LogMessages (class LogMessages, logError)
import Rtsv2App.Capability.Now (class Now)
import Rtsv2App.Data.Profile (Profile)
import Rtsv2App.Data.Username (Username)
import Rtsv2App.Env (UserEnv)
import Control.Monad.Reader (class MonadAsk, ask, asks)
import Data.Argonaut.Core (Json)
import Data.Either (Either(..), hush)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect.Aff.Bus as Bus
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Ref as Ref

mkRequest
  :: forall m r
   . MonadAff m
  => MonadAsk { apiUrl :: BaseURL | r } m
  => RequestOptions
  -> m (Maybe Json)
mkRequest opts = do
  { apiUrl } <- ask
  response <- liftAff $ request $ defaultRequest apiUrl Nothing opts
  pure $ hush response.body

mkAuthRequest
  :: forall m r
   . MonadAff m
  => MonadAsk { authUrl :: BaseURL | r } m
  => RequestOptions
  -> m (Maybe Json)
mkAuthRequest opts = do
  { authUrl } <- ask
  token <- liftEffect readToken
  response <- liftAff $ request $ defaultRequest authUrl token opts
  pure $ hush response.body

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

decode :: forall m a. LogMessages m => Now m => (Json -> Either String a) -> Maybe Json -> m (Maybe a)
decode _ Nothing = logError "Response malformed" *> pure Nothing 
decode decoder (Just json) = case decoder json of
  Left err -> logError err *> pure Nothing
  Right response -> pure (Just response)

decodeWithUser
  :: forall m a r
   . MonadEffect m
  => MonadAsk { userEnv :: UserEnv | r } m
  => LogMessages m 
  => Now m
  => (Maybe Username -> Json -> Either String a) 
  -> Maybe Json 
  -> m (Maybe a)
decodeWithUser decoder json = do
  maybeProfile <- (liftEffect <<< Ref.read) =<< asks _.userEnv.currentUser
  decode (decoder (_.username <$> maybeProfile)) json
