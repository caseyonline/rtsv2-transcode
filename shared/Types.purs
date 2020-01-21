module Shared.Types
       ( Load
       , ServerAddress(..)
       , RegionName(..)
       , PoPName(..)
       , ServerLoad(..)
       , ServerLocation(..)
       , LocatedServer(..)
       , IngestAggregatorPublicState
       , extractLocatedServer
       , toLoadedServer
       , toLocatedServer
       , locatedServerAddress
       , locatedServerPoP
       ) where

import Prelude

import Data.Newtype (class Newtype, unwrap)
import Data.Symbol (SProxy(..))
import Record as Record
import Shared.LlnwApiTypes (StreamDetails)
import Shared.Stream (StreamVariant)
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

newtype ServerLocation = ServerLocation { pop :: PoPName
                                        , region :: RegionName
                                        }
derive instance newtypeServerLocation :: Newtype ServerLocation _
derive newtype instance eqServerLocation :: Eq ServerLocation
derive newtype instance ordServerLocation :: Ord ServerLocation
derive newtype instance showServerLocation :: Show ServerLocation
derive newtype instance readForeignServerLocation :: ReadForeign ServerLocation
derive newtype instance writeForeignServerLocation :: WriteForeign ServerLocation


newtype LocatedServer = LocatedServer { address :: ServerAddress
                                      , pop :: PoPName
                                      , region :: RegionName
                                      }

derive instance newtypeLocatedServer :: Newtype LocatedServer _
derive newtype instance eqLocatedServer :: Eq LocatedServer
derive newtype instance ordLocatedServer :: Ord LocatedServer
derive newtype instance showLocatedServer :: Show LocatedServer
derive newtype instance readForeignLocatedServer :: ReadForeign LocatedServer
derive newtype instance writeForeignLocatedServer :: WriteForeign LocatedServer

newtype ServerLoad = ServerLoad { address :: ServerAddress
                                , pop :: PoPName
                                , region :: RegionName
                                , load :: Load
                                }
derive instance newtypeServerLoad :: Newtype ServerLoad _
derive newtype instance eqServerLoad :: Eq ServerLoad
derive newtype instance ordServerLoad :: Ord ServerLoad
derive newtype instance showServerLoad :: Show ServerLoad
derive newtype instance readForeignServerLoad :: ReadForeign ServerLoad
derive newtype instance writeForeignServerLoad :: WriteForeign ServerLoad

type IngestAggregatorPublicState
   = { streamDetails :: StreamDetails
     , activeStreamVariants :: Array { streamVariant :: StreamVariant
                                     , serverAddress :: ServerAddress
                                     }
     }


type ServerAddressRec = {address :: ServerAddress}
type LocationRec = { pop :: PoPName
                   , region :: RegionName
                   }

toLocatedServer :: ServerAddress -> ServerLocation -> LocatedServer
toLocatedServer sa (ServerLocation ls) =
  LocatedServer $ Record.insert address_ sa ls

toLoadedServer :: LocatedServer -> Load -> ServerLoad
toLoadedServer  (LocatedServer ls) load =
  ServerLoad $ Record.insert load_ load ls

extractLocatedServer :: ServerLoad -> LocatedServer
extractLocatedServer (ServerLoad sl) =
  LocatedServer $ Record.delete load_ sl

locatedServerPoP :: LocatedServer -> PoPName
locatedServerPoP = unwrap >>> _.pop

locatedServerAddress :: LocatedServer -> ServerAddress
locatedServerAddress = unwrap >>> _.address


--------------------------------------------------------------------------------
-- internal
--------------------------------------------------------------------------------
load_ = SProxy :: SProxy "load"
address_ = SProxy :: SProxy "address"
