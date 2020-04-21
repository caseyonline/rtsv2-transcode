module Helpers.Types where

import Prelude

import Effect.Aff (Aff)
import Data.Either (Either(..))
import Data.Newtype (class Newtype)
import Shared.Types (ServerAddress(..))
import Milkis as M


data Node = Node Int Int

type TestNode
  = { ifaceIndexString :: String
    , addr :: String
    , sysConfig :: String
    }
derive instance eqNode :: Eq Node

newtype NodeAddress = NodeAddress {address :: ServerAddress}
derive instance newtypeNodeAddress :: Newtype NodeAddress _

type PoPInfo
  = { name :: String
    , number :: Int
    , x :: Number
    , y :: Number
    }

type ResWithBody =
  { headers :: M.Headers
  , body :: String
  , statusCode :: Int
  }

type ToRecord a = Either String ResWithBody -> Aff (Either String a)
