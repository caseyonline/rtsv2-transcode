module Shared.Types
       ( Load(..)
       , ServerAddress(..)
       , RegionName(..)
       , PoPName(..)
       ) where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.Newtype (class Newtype)
import Foreign (unsafeToForeign)
import Simple.JSON (class ReadForeign, class WriteForeign)

newtype ServerAddress = ServerAddress String
derive instance newtypeServerAddress :: Newtype ServerAddress _
derive newtype instance eqServerAddress :: Eq ServerAddress
derive newtype instance ordServerAddress :: Ord ServerAddress
derive newtype instance showServerAddress :: Show ServerAddress
derive newtype instance readForeignServerAddress :: ReadForeign ServerAddress
derive newtype instance writeForeignServerAddress :: WriteForeign ServerAddress

newtype RegionName = RegionName String
derive instance newtypeRegionName :: Newtype RegionName _
derive newtype instance eqRegionName :: Eq RegionName
derive newtype instance ordRegionName :: Ord RegionName
derive newtype instance showRegionName :: Show RegionName
derive newtype instance readForeignRegionName :: ReadForeign RegionName
derive newtype instance writeForeignRegionName :: WriteForeign RegionName

newtype PoPName = PoPName String
derive instance newtypePoPName :: Newtype PoPName _
derive newtype instance eqPoPName :: Eq PoPName
derive newtype instance ordPoPName :: Ord PoPName
derive newtype instance showPoPName :: Show PoPName
derive newtype instance readForeignPoPName :: ReadForeign PoPName
derive newtype instance writeForeignPoPName :: WriteForeign PoPName

data Load = Idle
          | Busy
          | Overloaded
          | Critical
derive instance genericLoad :: Generic Load _

instance eqLoad :: Eq Load where
  eq = genericEq

instance showLoad :: Show Load where
  show = genericShow

instance ordLoad :: Ord Load where
  compare lhs rhs = compare (toNum lhs) (toNum rhs)

instance foreignLoad :: WriteForeign Load where
  writeImpl = unsafeToForeign <<< show

toNum :: Load -> Int
toNum Idle = 0
toNum Busy = 1
toNum Overloaded = 2
toNum Critical = 3
