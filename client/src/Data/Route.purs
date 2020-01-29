module Rtsv2App.Data.Route where

import Prelude hiding ((/))

import Data.Either (note)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Routing.Duplex (RouteDuplex', as, root)
import Routing.Duplex.Generic (noArgs, sum)
import Routing.Duplex.Generic.Syntax ((/))
import Rtsv2App.Data.Username (Username)
import Rtsv2App.Data.Username as Username
import Shared.Types (PoPName, parsePname, toStringPname)

data Route
  = Dashboard
  | Login
  | Register
  | Settings

derive instance genericRoute :: Generic Route _
derive instance eqRoute :: Eq Route
derive instance ordRoute :: Ord Route

instance showRoute :: Show Route where
  show = genericShow

routeCodec :: RouteDuplex' Route
routeCodec = root $ sum
  { "Dashboard": noArgs
  , "Login": "login" / noArgs
  , "Register": "register" / noArgs
  , "Settings": "settings" / noArgs
  }

-- | This combinator transforms a codec over `String` into one that operatos on the `Username` type.
uname :: RouteDuplex' String -> RouteDuplex' Username
uname = as Username.toString (Username.parse >>> note "Bad username")

pName :: RouteDuplex' String -> RouteDuplex' PoPName
pName = as toStringPname (parsePname >>> note "Bad username")
