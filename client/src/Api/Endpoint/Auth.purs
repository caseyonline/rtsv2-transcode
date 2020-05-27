module Rtsv2App.Api.Endpoint.Auth where

import Prelude hiding ((/))

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Routing.Duplex (RouteDuplex', root, segment)
import Routing.Duplex.Generic (noArgs, sum)
import Routing.Duplex.Generic.Syntax ((/))
import Shared.Rtsv2.Router.Endpoint.Class (class RoutedEndpoint)
import Shared.Rtsv2.Router.Endpoint.Combinators (uName)
import Shared.Rtsv2.Types (Username)

data Endpoint
  = LoginE
  | UserE
  | UsersE
  | ProfilesE Username

derive instance genericEndpoint :: Generic Endpoint _

instance showEndpoint :: Show Endpoint where
  show = genericShow

endpoint :: RouteDuplex' Endpoint
endpoint = root $ sum
  { "LoginE"                                           : "api" / "users" / "login" / noArgs
  , "UserE"                                            : "api" / "user" / noArgs
  , "UsersE"                                           : "api" / "users" / noArgs
  , "ProfilesE"                                        : "api" / "profile" / uName segment
  }

instance authEndpoint :: RoutedEndpoint Endpoint where
  getRoute _ = endpoint