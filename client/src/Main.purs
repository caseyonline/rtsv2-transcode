module Main where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse_)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Aff as Aff
import Effect.Aff.Bus as Bus
import Effect.Ref as Ref
import Halogen (liftAff, liftEffect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.VDom.Driver (runUI)
import Milkis as M
import Routing.Duplex (parse)
import Routing.Hash (matchesWith)
import Rtsv2App.Api.Endpoint (Endpoint(..))
import Rtsv2App.Api.Request (BaseURL(..), RequestMethod(..), ProfileJson, defaultRequest, fetch, printBaseUrl, readToken)
import Rtsv2App.AppM (runAppM)
import Rtsv2App.Component.Router as Router
import Rtsv2App.Data.Route (routeCodec)
import Rtsv2App.Env (LogLevel(..), UserEnv, Env)
import Simple.JSON as JSON


main :: Effect Unit
main = HA.runHalogenAff do

  body <- HA.awaitBody
  let
    -- TODO: will need to change this when we point to a local OAUTH
    authUrl = AuthURL "https://conduit.productionready.io"
    apiUrl = ApiUrl "https://conduit.productionready.io"

    logLevel = Dev

  -- default currentUser Ref to Nothing when starting app
  currentUser <- liftEffect $ Ref.new Nothing

  -- new bus to broadcast updates when the value of the current user changes;
  userBus <- liftEffect Bus.make 

  -- Attempt to fill the reference with the user profile associated with the token in
  -- local storage (if there is one). Read the token, request the user's profile if it can, and
  -- if it gets a valid result, write it to our mutable reference.
  liftEffect readToken >>= traverse_ \token -> do
    let requestOptions = { endpoint: User, method: Get }
    response <- liftAff $ Aff.attempt $ fetch (M.URL $ printBaseUrl $ authUrl ) $ defaultRequest (Just token) requestOptions
    case response of
      Left _    -> mempty
      Right res -> do
        r <- liftAff $ M.text res
        case JSON.readJSON r of
          Left e -> mempty
          Right (result :: ProfileJson) -> do
            liftEffect $ Ref.write (Just result.user) currentUser
            pure unit

  let
    environment :: Env
    environment = { apiUrl, authUrl, logLevel, userEnv }
      where
      userEnv :: UserEnv
      userEnv = { currentUser, userBus }

  -- Produce a proper root component for Halogen to run. combining `hoist`, `runAppM`, our environment, and our router component
    rootComponent :: H.Component HH.HTML Router.Query {} Void Aff
    rootComponent = H.hoist (runAppM environment) Router.component

  halogenIO <- runUI rootComponent {} body

  -- Nnotify the router any time the location changes in the URL using hash-based routing.
  void $ liftEffect $ matchesWith (parse routeCodec) \old new ->
    when (old /= Just new) do
      launchAff_ $ halogenIO.query $ H.tell $ Router.Navigate new
      
  pure unit
