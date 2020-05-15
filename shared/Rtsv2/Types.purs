module Shared.Rtsv2.Types
       ( DeliverTo(..)
       , Canary(..)
       , RunState(..)
       , GeoLoc(..)
       , LeaderGeoLoc(..)
       , CurrentLoad(..)
       , AcceptingRequests(..)
       , PoPName(..)
       , SourceRoute
       , RegionName(..)
       , Server(..)
       , ServerAddress(..)
       , ServerLoad(..)
       , ServerLocation(..)
       , Health(..)
       , ServerRec
       , RelayServer(..)
       , EgestServer(..)
       , Username(..)
       , CheckBoxState(..)
       , JsonLdContextType(..)
       , SpecInt(..)
       , NetworkKbps(..)
       , Percentage(..)
       , LocationResp
       , FailureReason(..)
       , LocalOrRemote(..)
       , ResourceResp(..)
       , ResourceFailed(..)
       , RegistrationResp(..)

       , fromLocalOrRemote
       , toServerLoad
       , toServerLocation
       , serverLoadToServer
       , extractAddress
       , extractPoP
       , toStringPname
       , parsePname
       , minLoad
       , maxLoad
       ) where

import Prelude

import Control.Monad.Except (except)
import Data.Either (Either, note)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.List.NonEmpty (singleton)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Symbol (SProxy(..))
import Foreign (ForeignError(..), readString, unsafeToForeign)
import Kishimen (genericSumToVariant, variantToGenericSum)
import Record as Record
import Shared.Rtsv2.Agent (Agent)
import Shared.Rtsv2.Stream (SlotId)
import Simple.JSON (class ReadForeign, class WriteForeign, readImpl, writeImpl)

newtype NetworkKbps = NetworkKbps Int
newtype SpecInt = SpecInt Number
newtype Percentage = Percentage Number

data JsonLdContextType = ServerContext
                       | ServerAddressContext
                       | DeliverToContext
                       | TimedRouteNeighbourContext
                       | ActiveIngestLocationContext
                       | EgestStatsContext
                       | IntraPoPStateContext
                       | IngestAggregatorStateContext
                       | StreamRelayStateContext
                       | IngestStateContext
                       | NodeManagerStateContext
                       | HealthContext

data Canary = Live
            | Canary

data RunState = Active
              | PassiveDrain
              | ForceDrain
              | OutOfOperation

data Health = Perfect
            | Excellent
            | Good
            | Poor
            | Critical
            | NA

newtype ServerAddress = ServerAddress String

newtype RegionName = RegionName String

newtype PoPName = PoPName String

type SourceRoute = Array PoPName

newtype GeoLoc = GeoLoc String

newtype CurrentLoad = CurrentLoad { cpu :: Percentage
                                  , network :: NetworkKbps
                                  }

newtype AcceptingRequests = AcceptingRequests Boolean

newtype ServerLocation = ServerLocation { pop :: PoPName
                                        , region :: RegionName
                                        }

type ServerRec l = { address :: ServerAddress
                   , pop :: PoPName
                   , region :: RegionName
                   , maxCpuCapacity :: SpecInt
                   , maxNetworkCapacity :: NetworkKbps
                   , capabilityTags :: Array String
                   , agents :: Array Agent
                     | l
                   }

newtype Server = Server (ServerRec ())
newtype RelayServer = Relay (ServerRec ())
newtype EgestServer = Egest (ServerRec ())

type DeliverTo serverType
  = { server :: serverType
    , port :: Int
    }

newtype ServerLoad = ServerLoad (ServerRec (load :: CurrentLoad, acceptingRequests :: AcceptingRequests))

data FailureReason
  = NotFound
  | NoResource

instance semigroupFailureReason :: Semigroup FailureReason where
  append lhs rhs = rhs

data LocalOrRemote a
  = Local a
  | Remote a
derive instance functorLocalOrRemoteF :: Functor LocalOrRemote

type LocationResp = (Either FailureReason (LocalOrRemote Server))

fromLocalOrRemote :: forall a. LocalOrRemote a -> a
fromLocalOrRemote (Local a) = a
fromLocalOrRemote (Remote a) = a

--------------------------------------------------------------------------------
-- API Types - maybe move me
--------------------------------------------------------------------------------
type ResourceResp a = Either ResourceFailed (LocalOrRemote a)

data ResourceFailed = NoCapacity
                    | LaunchFailed

type RegistrationResp = (Either FailureReason Unit)


toServerLoad :: Server -> CurrentLoad -> AcceptingRequests -> ServerLoad
toServerLoad  (Server ls) load acceptingRequests =
  ServerLoad $ Record.insert acceptingRequests_ acceptingRequests $ Record.insert load_ load ls

toServerLocation :: forall r a. Newtype a { pop :: PoPName
                                          , region :: RegionName | r } => a -> ServerLocation
toServerLocation = unwrap >>> (\{pop, region} -> ServerLocation {pop, region})

serverLoadToServer :: ServerLoad -> Server
serverLoadToServer (ServerLoad sl) =
  Server $ Record.delete acceptingRequests_ $ Record.delete load_ sl

extractPoP :: forall r a. Newtype a { pop :: PoPName | r } => a -> PoPName
extractPoP = unwrap >>> _.pop

extractAddress :: forall r a. Newtype a { address :: ServerAddress | r } => a -> ServerAddress
extractAddress = unwrap >>> _.address

minLoad :: CurrentLoad
minLoad = CurrentLoad { cpu: wrap 0.0
                      , network: wrap 0 }

maxLoad :: CurrentLoad
maxLoad = CurrentLoad { cpu: wrap 100.0
                      , network: wrap 2147483647 }


------------------------------------------------------------------------------
-- Type class derivations
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- Canary
instance showCanary :: Show Canary where
  show Live   = "live"
  show Canary = "canary"

derive instance genericCanary :: Generic Canary _
instance readForeignCanary :: ReadForeign Canary where
  readImpl = readString >=> parseString
    where
      error s = singleton (ForeignError (errorString s))
      parseString s = except $ note (error s) (toType s)
      toType "live" = pure Live
      toType "canary" = pure Canary
      toType unknown = Nothing
      errorString s = "Unknown Canary: " <> s

instance writeForeignCanary :: WriteForeign Canary where
  writeImpl = toString >>> unsafeToForeign
    where
      toString Live = "live"
      toString Canary = "canary"

------------------------------------------------------------------------------
-- RunState
derive instance genericRunState :: Generic RunState _
instance eqRunState :: Eq RunState where eq = genericEq
instance readForeignRunState :: ReadForeign RunState where
  readImpl = readString >=> parseString
    where
      error s = singleton (ForeignError (errorString s))
      parseString s = except $ note (error s) (toType s)
      toType "active" = pure Active
      toType "passiveDrain" = pure PassiveDrain
      toType "forceDrain" = pure ForceDrain
      toType "outOfOperation" = pure OutOfOperation
      toType unknown = Nothing
      errorString s = "Unknown RunState: " <> s

instance writeForeignRunState :: WriteForeign RunState where
  writeImpl = toString >>> unsafeToForeign
    where
      toString Active = "active"
      toString PassiveDrain = "passiveDrain"
      toString ForceDrain = "forceDrain"
      toString OutOfOperation = "outOfOperation"

------------------------------------------------------------------------------
-- JsonLdContextType
derive instance genericJsonLdContextType :: Generic JsonLdContextType _
instance showJsonLdContextType :: Show JsonLdContextType where show = genericShow

------------------------------------------------------------------------------
-- ResourceFailed
derive instance genericResourceFailed :: Generic ResourceFailed _
instance showResourceFailed :: Show ResourceFailed where show = genericShow

------------------------------------------------------------------------------
-- ServerAddress
derive instance newtypeServerAddress :: Newtype ServerAddress _
derive newtype instance eqServerAddress :: Eq ServerAddress
derive newtype instance ordServerAddress :: Ord ServerAddress
derive newtype instance showServerAddress :: Show ServerAddress
derive newtype instance readForeignServerAddress :: ReadForeign ServerAddress
derive newtype instance writeForeignServerAddress :: WriteForeign ServerAddress

------------------------------------------------------------------------------
-- RegionName
derive instance newtypeRegionName :: Newtype RegionName _
derive newtype instance eqRegionName :: Eq RegionName
derive newtype instance ordRegionName :: Ord RegionName
derive newtype instance showRegionName :: Show RegionName
derive newtype instance readForeignRegionName :: ReadForeign RegionName
derive newtype instance writeForeignRegionName :: WriteForeign RegionName

------------------------------------------------------------------------------
-- PoPName
derive instance genericPoPName :: Generic PoPName _
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

------------------------------------------------------------------------------
-- GeoLoc
derive instance newtypeGeoLoc :: Newtype GeoLoc _
derive newtype instance eqGeoLoc :: Eq GeoLoc
derive newtype instance ordGeoLoc :: Ord GeoLoc
derive newtype instance showGeoLoc :: Show GeoLoc
derive newtype instance readForeignGeoLoc :: ReadForeign GeoLoc
derive newtype instance writeForeignGeoLoc :: WriteForeign GeoLoc

------------------------------------------------------------------------------
-- AcceptingRequests
derive instance newtypeAcceptingRequests :: Newtype AcceptingRequests _
derive newtype instance eqAcceptingRequests :: Eq AcceptingRequests
derive newtype instance ordAcceptingRequests :: Ord AcceptingRequests
derive newtype instance showAcceptingRequests :: Show AcceptingRequests
derive newtype instance readForeignAcceptingRequests :: ReadForeign AcceptingRequests
derive newtype instance writeForeignAcceptingRequests :: WriteForeign AcceptingRequests

------------------------------------------------------------------------------
-- CurrentLoad
derive instance newtypeCurrentLoad :: Newtype CurrentLoad _
derive newtype instance eqCurrentLoad :: Eq CurrentLoad
derive newtype instance ordCurrentLoad :: Ord CurrentLoad
derive newtype instance showCurrentLoad :: Show CurrentLoad
derive newtype instance readForeignCurrentLoad :: ReadForeign CurrentLoad
derive newtype instance writeForeignCurrentLoad :: WriteForeign CurrentLoad

------------------------------------------------------------------------------
-- ServerLocation
derive instance newtypeServerLocation :: Newtype ServerLocation _
derive newtype instance eqServerLocation :: Eq ServerLocation
derive newtype instance ordServerLocation :: Ord ServerLocation
derive newtype instance showServerLocation :: Show ServerLocation
derive newtype instance readForeignServerLocation :: ReadForeign ServerLocation
derive newtype instance writeForeignServerLocation :: WriteForeign ServerLocation

------------------------------------------------------------------------------
-- Server
derive instance newtypeServer :: Newtype Server _
derive newtype instance eqServer :: Eq Server
derive newtype instance ordServer :: Ord Server
derive newtype instance showServer :: Show Server
derive newtype instance readForeignServer :: ReadForeign Server
derive newtype instance writeForeignServer :: WriteForeign Server

------------------------------------------------------------------------------
-- RelayServer
derive instance newtypeRelayServer :: Newtype RelayServer _
derive newtype instance eqRelayServer :: Eq RelayServer
derive newtype instance ordRelayServer :: Ord RelayServer
derive newtype instance showRelayServer :: Show RelayServer
derive newtype instance readForeignRelayServer :: ReadForeign RelayServer
derive newtype instance writeForeignRelayServer :: WriteForeign RelayServer

------------------------------------------------------------------------------
-- EgestServer
derive instance newtypeEgestServer :: Newtype EgestServer _
derive newtype instance eqEgestServer :: Eq EgestServer
derive newtype instance ordEgestServer :: Ord EgestServer
derive newtype instance showEgestServer :: Show EgestServer
derive newtype instance readForeignEgestServer :: ReadForeign EgestServer
derive newtype instance writeForeignEgestServer :: WriteForeign EgestServer

------------------------------------------------------------------------------
-- ServerLoad
derive instance newtypeServerLoad :: Newtype ServerLoad _
derive newtype instance eqServerLoad :: Eq ServerLoad
derive newtype instance ordServerLoad :: Ord ServerLoad
derive newtype instance showServerLoad :: Show ServerLoad
derive newtype instance readForeignServerLoad :: ReadForeign ServerLoad
derive newtype instance writeForeignServerLoad :: WriteForeign ServerLoad

------------------------------------------------------------------------------
-- NetworkKbps
derive instance newtypeNetworkKbps :: Newtype NetworkKbps _
derive instance genericNetworkKbps :: Generic NetworkKbps _
instance showNetworkKbps :: Show NetworkKbps where show = genericShow
instance eqNetworkKbps :: Eq NetworkKbps where eq = genericEq
derive newtype instance readForeignNetworkKbps :: ReadForeign NetworkKbps
derive newtype instance writeForeignNetworkKbps :: WriteForeign NetworkKbps
instance ordNetworkKbps :: Ord NetworkKbps where
  compare (NetworkKbps x) (NetworkKbps y) = compare x y
instance semigroupNetworkKbps :: Semigroup NetworkKbps where
  append (NetworkKbps x) (NetworkKbps y) = NetworkKbps (x + y)

------------------------------------------------------------------------------
-- SpecInt
derive instance newtypeSpecInt :: Newtype SpecInt _
derive instance genericSpecInt :: Generic SpecInt _
instance showSpecInt :: Show SpecInt where show = genericShow
instance eqSpecInt :: Eq SpecInt where eq = genericEq
derive newtype instance readForeignSpecInt :: ReadForeign SpecInt
derive newtype instance writeForeignSpecInt :: WriteForeign SpecInt
instance ordSpecInt :: Ord SpecInt where
  compare (SpecInt x) (SpecInt y) = compare x y

------------------------------------------------------------------------------
-- Percentage
derive instance newtypePercentage :: Newtype Percentage _
derive instance genericPercentage :: Generic Percentage _
instance showPercentage :: Show Percentage where show = genericShow
derive newtype instance readForeignPercentage :: ReadForeign Percentage
derive newtype instance writeForeignPercentage :: WriteForeign Percentage
derive newtype instance eqPercentage :: Eq Percentage
derive newtype instance ordPercentage :: Ord Percentage
instance semigroupPercentage :: Semigroup Percentage where
  append (Percentage x) (Percentage y) = Percentage (x + y)

------------------------------------------------------------------------------
-- Health
derive instance genericHealth :: Generic Health _
instance eqAgent :: Eq Health where eq = genericEq
instance showAgent :: Show Health where show = genericShow
instance foreignHealth :: WriteForeign Health where
  writeImpl = unsafeToForeign <<< show

------------------------------------------------------------------------------
-- FrontEnd Specific Types
type LeaderGeoLoc =
  { name  :: PoPName
  , coord :: Array Number
  }

type CheckBoxState =
  { slotId     :: Maybe SlotId
  , isSelected :: Boolean
  }

newtype Username = Username String

derive instance genericUsername :: Generic Username _
derive instance eqUsername :: Eq Username
derive instance ordUsername :: Ord Username

derive newtype instance readForeignUsername  :: ReadForeign Username
derive newtype instance writeForeignUsername :: WriteForeign Username

instance showUsername :: Show Username where
  show = genericShow


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
acceptingRequests_ = SProxy :: SProxy "acceptingRequests"
address_ = SProxy :: SProxy "address"
