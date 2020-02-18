module Rtsv2App.Api.Endpoint where

import Prelude hiding ((/))

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Routing.Duplex (RouteDuplex', prefix, root, segment)
import Routing.Duplex.Generic (noArgs, sum)
import Routing.Duplex.Generic.Syntax ((/))
import Rtsv2App.Data.Route (uname, popName)
import Rtsv2App.Data.Username (Username)
import Shared.Types (PoPName)


-------------------------------------------------------------------------------
-- API RouteDuplex Endpoints
-------------------------------------------------------------------------------
data Endpoint
  = LoginE
  | UserE
  | UsersE
  | TimedRoutesE PoPName
  | PopDefinitionE
  | PublicState
  | ProfilesE Username

derive instance genericEndpoint :: Generic Endpoint _

instance showEndpoint :: Show Endpoint where
  show = genericShow

endpointCodec :: RouteDuplex' Endpoint
endpointCodec = root $ prefix "api" $ sum
  { "LoginE"         : "users" / "login" / noArgs
  , "UserE"          : "user" / noArgs
  , "UsersE"         : "users" / noArgs
  , "TimedRoutesE"   : "timedRoutes" / popName segment
  , "PopDefinitionE" : "popDefinition" / noArgs
  , "PublicState"    : "state" / noArgs
  -- automatically create query parameters
  , "ProfilesE"      : "profiles" / uname segment
  }
