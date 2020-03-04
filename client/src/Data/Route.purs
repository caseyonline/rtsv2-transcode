module Rtsv2App.Data.Route where

import Prelude hiding ((/))

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Routing.Duplex (RouteDuplex', root, segment)
import Routing.Duplex.Generic (noArgs, sum)
import Routing.Duplex.Generic.Syntax ((/))
import Shared.Router.Endpoint (popName)
import Shared.Types (PoPName)


-------------------------------------------------------------------------------
-- App Routes
-------------------------------------------------------------------------------
data Route
  = DashboardR
  | PoPDashboardR PoPName
  | LoginR
  | RegisterR
  | SettingsR

derive instance genericRoute :: Generic Route _
derive instance eqRoute :: Eq Route
derive instance ordRoute :: Ord Route

instance showRoute :: Show Route where
  show = genericShow

routeCodec :: RouteDuplex' Route
routeCodec = root $ sum
  { "DashboardR": noArgs
  , "LoginR": "login" / noArgs
  , "PoPDashboardR": "pop" / popName segment
  , "RegisterR": "register" / noArgs
  , "SettingsR": "settings" / noArgs
  }
