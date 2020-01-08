module Shared.Types
       ( Load
       , ServerAddress(..)
       , RegionName(..)
       , PoPName(..)
       , ServerLoad(..)
       , ServerLocation(..)
       , LocatedServer(..)
       , locatedServerAddress
       , locatedServerLocation
       ) where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Newtype (class Newtype)
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

newtype Load = Load Number
derive instance newtypeLoad :: Newtype Load _
derive newtype instance eqLoad :: Eq Load
derive newtype instance ordLoad :: Ord Load
derive newtype instance showLoad :: Show Load
derive newtype instance readForeignLoad :: ReadForeign Load
derive newtype instance writeForeignLoad :: WriteForeign Load

data ServerLocation = ServerLocation PoPName RegionName
derive instance genericServerLocation :: Generic ServerLocation _
instance eqServerLocation :: Eq ServerLocation where
  eq = genericEq

data LocatedServer = LocatedServer ServerAddress ServerLocation
derive instance genericLocatedServer :: Generic LocatedServer _
instance eqLocatedServer :: Eq LocatedServer where
  eq = genericEq

locatedServerAddress :: LocatedServer -> ServerAddress
locatedServerAddress (LocatedServer address _location) = address

locatedServerLocation :: LocatedServer -> ServerLocation
locatedServerLocation (LocatedServer _address location) = location

data ServerLoad = ServerLoad LocatedServer Load
derive instance genericServerLoad :: Generic ServerLoad _
instance eqServerLoad :: Eq ServerLoad where
  eq = genericEq
