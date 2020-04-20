module Helpers.Types where

import Prelude

import Data.Newtype (class Newtype)
import Shared.Types (ServerAddress(..))

-- | Node
data Node = Node Int Int

type TestNode
  = { ifaceIndexString :: String
    , addr :: String
    , sysConfig :: String
    }
derive instance eqNode :: Eq Node

newtype NodeAddress = NodeAddress {address :: ServerAddress}
derive instance newtypeNodeAddress :: Newtype NodeAddress _

-- | PoP
type PoPInfo
  = { name :: String
    , number :: Int
    , x :: Number
    , y :: Number
    }
