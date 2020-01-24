module Shared.Types
       ( Load
       , ServerAddress(..)
       , RegionName(..)
       , PoPName(..)
       , ServerLoad(..)
       , ServerLocation(..)
       , Server(..)
       , ServerRec
       , RelayServer(..)
       , EgestServer(..)
       , IngestAggregatorPublicState
       , EgestLocation(..)
       , FailureReason(..)
       , APIResp(..)
       , toServer
       , toServerLoad
       , serverLoadToServer
       , extractAddress
       , extractPoP
       ) where

import Prelude

import Data.Either (Either)
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




type ServerRec = { address :: ServerAddress
                 , pop :: PoPName
                 , region :: RegionName
                 }

newtype Server = Server ServerRec
newtype RelayServer = Relay ServerRec
newtype EgestServer = Egest ServerRec


-- newtype AggregatorServer = Aggregator Server
-- newtype IdleServerServer = Idle Server

derive instance newtypeServer :: Newtype Server _
derive newtype instance eqServer :: Eq Server
derive newtype instance ordServer :: Ord Server
derive newtype instance showServer :: Show Server
derive newtype instance readForeignServer :: ReadForeign Server
derive newtype instance writeForeignServer :: WriteForeign Server

derive instance newtypeRelayServer :: Newtype RelayServer _
derive newtype instance eqRelayServer :: Eq RelayServer
derive newtype instance ordRelayServer :: Ord RelayServer
derive newtype instance showRelayServer :: Show RelayServer
derive newtype instance readForeignRelayServer :: ReadForeign RelayServer
derive newtype instance writeForeignRelayServer :: WriteForeign RelayServer

derive instance newtypeEgestServer :: Newtype EgestServer _
derive newtype instance eqEgestServer :: Eq EgestServer
derive newtype instance ordEgestServer :: Ord EgestServer
derive newtype instance showEgestServer :: Show EgestServer
derive newtype instance readForeignEgestServer :: ReadForeign EgestServer
derive newtype instance writeForeignEgestServer :: WriteForeign EgestServer




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


--------------------------------------------------------------------------------
-- API Types - maybe move me
--------------------------------------------------------------------------------
data EgestLocation
  = Local
  | Remote ServerAddress

data FailureReason
  = NotFound
  | NoResource

type APIResp = (Either FailureReason Unit)


toServer :: ServerAddress -> ServerLocation -> Server
toServer sa (ServerLocation ls) =
  Server $ Record.insert address_ sa ls

toServerLoad :: Server -> Load -> ServerLoad
toServerLoad  (Server ls) load =
  ServerLoad $ Record.insert load_ load ls

serverLoadToServer :: ServerLoad -> Server
serverLoadToServer (ServerLoad sl) =
  Server $ Record.delete load_ sl

extractPoP :: forall r a. Newtype a { pop :: PoPName | r } => a -> PoPName
extractPoP = unwrap >>> _.pop

extractAddress :: forall r a. Newtype a { address :: ServerAddress | r } => a -> ServerAddress
extractAddress = unwrap >>> _.address

--------------------------------------------------------------------------------
-- internal
--------------------------------------------------------------------------------
load_ = SProxy :: SProxy "load"
address_ = SProxy :: SProxy "address"
