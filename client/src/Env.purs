module Rtsv2App.Env where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Effect (Effect)
import Effect.Aff.Bus (BusRW)
import Effect.Ref (Ref)
import Rtsv2App.Data.Profile (Profile)
import Shared.Types.Agent.State (PoPDefinition)
import Web.HTML (window)
import Web.HTML.Location as L
import Web.HTML.Window as Window

type Env =
  { logLevel  :: LogLevel
  , urlEnv    :: UrlEnv
  , userEnv   :: UserEnv
  , popDefEnv :: PoPDefEnv
  }

data LogLevel = Dev | Prod

derive instance eqLogLevel :: Eq LogLevel
derive instance ordLogLevel :: Ord LogLevel

type UserEnv =
  { currentUser :: Ref (Maybe Profile)
  , userBus :: BusRW (Maybe Profile)
  }

type PoPDefEnv =
  { popDefinition :: Ref (Maybe (PoPDefinition Array)) }

newtype CurHostUrl = CurHostUrl String
derive instance genericCurHostUrl :: Generic CurHostUrl _
derive instance newtypeCurHostUrl :: Newtype CurHostUrl _

newtype AuthUrl = AuthUrl String
derive instance genericAuthUrl :: Generic AuthUrl _
derive instance newtypeAuthUrl :: Newtype AuthUrl _

type UrlEnv =
  { curHostUrl :: CurHostUrl
  , authUrl :: AuthUrl
  }

getCurOrigin :: Effect String
getCurOrigin = window >>= Window.location >>= L.origin
