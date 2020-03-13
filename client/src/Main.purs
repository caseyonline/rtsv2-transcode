module Main where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse_)
import Debug.Trace (traceM)
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
import Rtsv2App.Api.Request (ProfileJson, RequestMethod(..), fetchReq, printUrl, readToken, withResponse)
import Rtsv2App.AppM (runAppM)
import Rtsv2App.Component.Router as Router
import Rtsv2App.Data.Route (routeCodec)
import Rtsv2App.Env (AuthUrl(..), CurHostUrl(..), Env, LogLevel(..), UrlEnv, UserEnv, PoPDefEnv, getCurOrigin)
import Shared.Router.Endpoint (Endpoint(..))
import Shared.Types.Agent.State (PoPDefinition)


main :: Effect Unit
main = HA.runHalogenAff do

  body <- HA.awaitBody
  let
    logLevel = Dev
    -- TODO: this will need changing to our Auth
    authUrl = AuthUrl "https://conduit.productionready.io"
    -- html class for all pages
    htmlClass = "has-aside-left has-aside-mobile-transition has-navbar-fixed-top has-aside-expanded"

  -- | initialising all of the Global Ref states
  curHostUrl          <- liftEffect getCurOrigin
  currentUser         <- liftEffect $ Ref.new Nothing
  popDefinition       <- liftEffect $ Ref.new Nothing
  transPoPLeaders     <- liftEffect $ Ref.new mempty
  aggregatorLocations <- liftEffect $ Ref.new mempty
  geoLocations        <- liftEffect $ Ref.new mempty
  notices             <- liftEffect $ Ref.new mempty

  -- new bus to broadcast updates when the value of the current user changes;
  userBus <- liftEffect Bus.make

  liftEffect readToken >>= traverse_ \token -> do
    response <- liftAff $ Aff.attempt $ fetchReq (M.URL $ printUrl (CurHostUrl curHostUrl) PoPDefinitionE) Nothing Get
    popDef <- withResponse response \(result :: PoPDefinition Array) -> result
    case popDef of
      Left err -> traceM err -- need to do some proper error handling here
      Right pd -> liftEffect $ Ref.write (Just pd) popDefinition

  -- Attempt to fill the reference with the user profile associated with the token in
  -- local storage (if there is one). Read the token, request the user's profile if it can, and
  -- if it gets a valid result, write it to our mutable reference.
  liftEffect readToken >>= traverse_ \token -> do
    response <- liftAff $ Aff.attempt $ fetchReq (M.URL $ printUrl authUrl UserE) (Just token) Get
    user <- withResponse response \(result :: ProfileJson) -> result.user
    case user of
      Left err -> traceM err -- need to do some proper error handling here
      Right u -> liftEffect $ Ref.write (Just u) currentUser

  let
    environment :: Env
    environment = { urlEnv, logLevel, userEnv, popDefEnv }
      where
      urlEnv :: UrlEnv
      urlEnv = { curHostUrl: (CurHostUrl curHostUrl), authUrl, htmlClass }

      userEnv :: UserEnv
      userEnv = { currentUser, userBus, notices }

      popDefEnv :: PoPDefEnv
      popDefEnv = { popDefinition
                  , transPoPLeaders
                  , aggregatorLocations
                  , geoLocations
                  }
  -- Produce a proper root component for Halogen to run. combining `hoist`, `runAppM`, our environment, and our router component
    rootComponent :: H.Component HH.HTML Router.Query {} Void Aff
    rootComponent = H.hoist (runAppM environment) Router.component

  halogenIO <- runUI rootComponent {} body

  -- Nnotify the router any time the location changes in the URL using hash-based routing.
  void $ liftEffect $ matchesWith (parse routeCodec) \old new ->
    when (old /= Just new) do
      launchAff_ $ halogenIO.query $ H.tell $ Router.Navigate new
      
  pure unit
