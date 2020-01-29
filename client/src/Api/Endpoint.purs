module Rtsv2App.Api.Endpoint where

import Prelude hiding ((/))

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Routing.Duplex (RouteDuplex', prefix, root, segment)
import Routing.Duplex.Generic (noArgs, sum)
import Routing.Duplex.Generic.Syntax ((/))
import Rtsv2App.Data.Route (uname, pName)
import Rtsv2App.Data.Username (Username)
import Shared.Types (PoPName)

type PaginationRep =
  ( limit :: Maybe Int
  , offset :: Maybe Int
  )

type Pagination = { | PaginationRep  }

type ArticleParams =
  { tag :: Maybe String
  , author :: Maybe Username
  , favorited :: Maybe Username
  | PaginationRep
  }

noArticleParams :: ArticleParams
noArticleParams =
  { tag: Nothing
  , author: Nothing
  , favorited: Nothing
  , offset: Nothing
  , limit: Nothing
  }

data Endpoint
  = Login
  | User
  | Users
  | TimedRoutes PoPName
  | Profiles Username

derive instance genericEndpoint :: Generic Endpoint _

instance showEndpoint :: Show Endpoint where
  show = genericShow

endpointCodec :: RouteDuplex' Endpoint
endpointCodec = root $ prefix "api" $ sum
  { "Login"         : "users" / "login" / noArgs
  , "User"          : "user" / noArgs
  , "Users"         : "users" / noArgs
  , "TimedRoutes"   : "timedRoutes" / pName segment
  -- automatically create query parameters
  , "Profiles"      : "profiles" / uname segment
  }
