module Shared.Types
       ( EgestServer(..)
       , GeoLoc(..)
       , LeaderGeoLoc(..)
       , Load
       , Milliseconds(..)
       , PoPName(..)
       , PoPSelectedInfo(..)
       , RegionName(..)
       , RelayServer(..)
       , Server(..)
       , ServerAddress(..)
       , ServerLoad(..)
       , ServerLocation(..)
       , ServerRec
       , Url
       , toServer
       , toServerLoad
       , serverLoadToServer
       , extractAddress
       , extractPoP
       , toStringPname
       , parsePname
       ) where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Symbol (SProxy(..))
import Record as Record
import Shared.Stream (SlotId)
import Simple.JSON (class ReadForeign, class WriteForeign)

-- TODO - find a place for these utility types to live (a la id3as_common?)
-- | A duration measured in milliseconds.
newtype Milliseconds = Milliseconds Int

derive instance newtypeMilliseconds :: Newtype Milliseconds _
derive newtype instance eqMilliseconds :: Eq Milliseconds
derive newtype instance ordMilliseconds :: Ord Milliseconds
derive newtype instance readForeignMilliseconds :: ReadForeign Milliseconds
derive newtype instance writeForeignMilliseconds :: WriteForeign Milliseconds
instance semigroupMilliseconds :: Semigroup Milliseconds where
  append (Milliseconds x) (Milliseconds y) = Milliseconds (x + y)

instance semiringMilliseconds :: Semiring Milliseconds where
  add (Milliseconds x) (Milliseconds y) = Milliseconds (x + y)
  zero = Milliseconds 0
  mul (Milliseconds x) (Milliseconds y) = Milliseconds (x * y)
  one = Milliseconds 1

instance ringMilliseconds :: Ring Milliseconds where
  sub (Milliseconds x) (Milliseconds y) = Milliseconds (x - y)

instance monoidMilliseconds :: Monoid Milliseconds where
  mempty = Milliseconds 0

instance showMilliseconds :: Show Milliseconds where
  show (Milliseconds n) = "(Milliseconds " <> show n <> ")"

-- | Url type
newtype Url = Url String
derive instance newtypeURL :: Newtype Url _
derive newtype instance eqURL :: Eq Url
derive newtype instance ordURL :: Ord Url

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
derive instance genericUsername :: Generic PoPName _
derive instance newtypePoPName :: Newtype PoPName _
derive newtype instance eqPoPName :: Eq PoPName
derive newtype instance ordPoPName :: Ord PoPName
derive newtype instance showPoPName :: Show PoPName
derive newtype instance readForeignPoPName :: ReadForeign PoPName
derive newtype instance writeForeignPoPName :: WriteForeign PoPName

parsePname :: String -> Maybe PoPName
parsePname "" = Nothing
parsePname str = Just (PoPName str)

toStringPname :: PoPName -> String
toStringPname (PoPName str) = str

newtype GeoLoc = GeoLoc String
derive instance newtypeGeoLoc :: Newtype GeoLoc _
derive newtype instance eqGeoLoc :: Eq GeoLoc
derive newtype instance ordGeoLoc :: Ord GeoLoc
derive newtype instance showGeoLoc :: Show GeoLoc
derive newtype instance readForeignGeoLoc :: ReadForeign GeoLoc
derive newtype instance writeForeignGeoLoc :: WriteForeign GeoLoc

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

type LeaderGeoLoc =
  { name   :: PoPName
  , coords :: Array Number
  }

type PoPSelectedInfo =
    { selectedPoPName :: Maybe PoPName
    , selectedSlotId  :: Maybe SlotId
    , selectedAddress :: Maybe ServerAddress
    }

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

------------------------------------------------------------------------------
-- RTMP Client Metadata - currently there's erlang code in rtsv2_rtmp_ingest_handler that
-- knows these types - so if you change these, then you need to change that
------------------------------------------------------------------------------
------------------------------------------------------------------------------
-- Workflow Metrics - currently there's erlang code in IngestStats.erl that
-- knows these types - so if you change these, then you need to change that
------------------------------------------------------------------------------
-- data MetricValue = MetricInt Int
--                  | MetricFloat Number
--                  | MetricString String

-- derive instance genericMetricValue :: Generic MetricValue _
-- derive instance eqMetricValue :: Eq MetricValue
-- instance showMetricValue :: Show MetricValue where show = genericShow
-- instance readMetricValue :: ReadForeign MetricValue where readImpl = untaggedSumRep
-- instance writeForeignMetricValue :: WriteForeign MetricValue where
--   writeImpl (MetricInt int) = writeImpl int
--   writeImpl (MetricFloat float) = writeImpl float
--   writeImpl (MetricString string) = writeImpl string

-- newtype MetricTag = MetricTag {name :: String, value :: String}

-- derive instance newtypeMetricTag :: Newtype MetricTag _
-- derive newtype instance eqMetricTag :: Eq MetricTag
-- derive newtype instance showMetricTag :: Show MetricTag
-- derive newtype instance readForeignMetricTag :: ReadForeign MetricTag
-- derive newtype instance writeForeignMetricTag :: WriteForeign MetricTag

-- newtype MetricData f = MetricData
--                        { name :: String
--                        , displayName :: String
--                        , value :: MetricValue
--                        , tags :: Container f MetricTag
--                        }

-- derive instance newtypeMetricData :: Newtype (MetricData f) _
-- derive newtype instance eqMetricData :: Eq (f MetricTag) => Eq (MetricData f)
-- derive newtype instance showMetricData :: Show (f MetricTag) => Show (MetricData f)
-- derive newtype instance readForeignMetricData :: Unfoldable f => ReadForeign (MetricData f)
-- derive newtype instance writeForeignMetricData :: Foldable f => WriteForeign (MetricData f)

-- data WorkflowMetric f = Counter (MetricData f)
--                       | Gauge (MetricData f)
--                       | Text (MetricData f)

-- derive instance genericWorkflowMetric :: Generic (WorkflowMetric f) _
-- derive instance eqWorkflowMetric :: Eq (f MetricTag) => Eq (WorkflowMetric f)
-- instance showWorkflowMetric :: Show (f MetricTag) => Show (WorkflowMetric f) where show = genericShow
-- instance readWorkflowMetric :: (Unfoldable f) => ReadForeign (WorkflowMetric f) where readImpl = taggedSumRep
-- instance writeForeignWorkflowMetric :: (Foldable f) => WriteForeign (WorkflowMetric f) where
--   writeImpl (Counter metric) = writeImpl metric
--   writeImpl (Gauge metric) = writeImpl metric
--   writeImpl (Text metric) = writeImpl metric

-- -- Could possibly treat 'f a' as a single thing and have a Foldable<a> constraint in the readForeign etc
-- newtype Container f a = Cont (f a)
-- derive instance newtypeContainer :: Newtype (Container f a) _
-- derive newtype instance showContainer :: Show (f a) => Show (Container f a)
-- derive newtype instance eqContainer :: Eq (f a) => Eq (Container f a)
-- instance readForeignContainer :: (Unfoldable f, ReadForeign a) => ReadForeign (Container f a) where
--   readImpl f =
--     let
--       array :: F (Array a)
--       array = readImpl f
--     in
--      Cont <$> toUnfoldable <$> array

-- instance writeForeignContainer :: (Foldable f, WriteForeign a) => WriteForeign (Container f a) where
--   writeImpl (Cont list) =
--     let
--       array = fromFoldable
--     in
--      writeImpl (array list)

--------------------------------------------------------------------------------
-- internal
--------------------------------------------------------------------------------
load_ = SProxy :: SProxy "load"
address_ = SProxy :: SProxy "address"
