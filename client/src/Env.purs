module Rtsv2App.Env where

import Prelude

import Data.Maybe (Maybe)
import Effect (Effect)
import Effect.Aff.Bus (BusRW)
import Effect.Ref (Ref)
import Rtsv2App.Api.Request (BaseURL)
import Rtsv2App.Data.Profile (Profile)
import Web.HTML (window)
import Web.HTML.Location as L
import Web.HTML.Window as Window

type Env =
  { logLevel :: LogLevel
  , apiUrl :: BaseURL
  , authUrl :: BaseURL
  , userEnv :: UserEnv
  }

data LogLevel = Dev | Prod

derive instance eqLogLevel :: Eq LogLevel
derive instance ordLogLevel :: Ord LogLevel

type UserEnv =
  { currentUser :: Ref (Maybe Profile)
  , userBus :: BusRW (Maybe Profile)
  }


getCurOrigin :: Effect String
getCurOrigin = window >>= Window.location >>= L.origin
