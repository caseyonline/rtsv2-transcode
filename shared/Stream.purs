module Shared.Stream
  ( ShortName(..)
  , StreamId(..)
  , StreamIdAndRole(..)
  , StreamRole(..)
  , StreamVariant(..)
  , StreamAndVariant(..)
  , AgentKey(..)
  , EgestKey(..)
  , AggregatorKey(..)
  , RelayKey(..)
  , IngestKey(..)

  , agentKeyToAggregatorKey
  , aggregatorKeyToAgentKey
  , ingestKeyToAggregatorKey
  , ingestKeyToVariant

  , toStreamId
  , toVariant
  ) where

import Prelude

import Control.Apply (lift2)
import Control.Monad.Except (except)
import Data.Array ((!!))
import Data.Either (note)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Ord (genericCompare)
import Data.Generic.Rep.Show (genericShow)
import Data.List.NonEmpty (singleton)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.String (Pattern(..), split)
import Foreign (ForeignError(..), readString, unsafeToForeign)
import Simple.JSON (class ReadForeign, class WriteForeign)

newtype ShortName = ShortName String
derive instance genericShortName :: Generic ShortName _
derive instance newtypeShortName :: Newtype ShortName _

derive newtype instance readForeignShortName :: ReadForeign ShortName
derive newtype instance writeForeignShortName :: WriteForeign ShortName

instance eqShortName :: Eq ShortName where
  eq = genericEq

instance compareShortName :: Ord ShortName where
  compare = genericCompare

instance showShortName :: Show ShortName where
  show = genericShow


newtype StreamId = StreamId String
derive instance genericStreamId :: Generic StreamId _
derive instance newtypeStreamId :: Newtype StreamId _
derive newtype instance readForeignStreamId :: ReadForeign StreamId
derive newtype instance writeForeignStreamId :: WriteForeign StreamId

instance eqStreamId :: Eq StreamId where
  eq = genericEq

instance compareStreamId :: Ord StreamId where
  compare = genericCompare

instance showStreamId :: Show StreamId where
  show = genericShow

data StreamIdAndRole = StreamIdAndRole StreamId StreamRole
derive instance eqStreamIdAndRole :: Eq StreamIdAndRole
derive instance ordStreamIdAndRole :: Ord StreamIdAndRole

newtype EgestKey = EgestKey StreamId
data AggregatorKey = AggregatorKey StreamId StreamRole
derive instance eqAggregatorKey :: Eq AggregatorKey
derive instance ordAggregatorKey :: Ord AggregatorKey

data RelayKey = RelayKey StreamId StreamRole
data IngestKey = IngestKey StreamId StreamRole StreamVariant


aggregatorKeyToAgentKey :: AggregatorKey -> AgentKey
aggregatorKeyToAgentKey (AggregatorKey streamId streamRole) = AgentKey streamId streamRole

data AgentKey = AgentKey StreamId StreamRole
derive instance eqAgentKey :: Eq AgentKey
derive instance ordAgentKey :: Ord AgentKey


newtype StreamVariant = StreamVariant String
derive instance genericStreamVariant :: Generic StreamVariant _
derive instance newtypeStreamVariant :: Newtype StreamVariant _
derive newtype instance readForeignStreamVariant :: ReadForeign StreamVariant
derive newtype instance writeForeignStreamVariant :: WriteForeign StreamVariant


ingestKeyToAggregatorKey :: IngestKey -> AggregatorKey
ingestKeyToAggregatorKey (IngestKey streamId streamRole streamVariant) = (AggregatorKey streamId streamRole)


agentKeyToAggregatorKey :: AgentKey -> AggregatorKey
agentKeyToAggregatorKey (AgentKey streamId streamRole) = AggregatorKey streamId streamRole

ingestKeyToVariant :: IngestKey -> StreamVariant
ingestKeyToVariant (IngestKey streamId streamRole streamVariant) = streamVariant

data StreamAndVariant = StreamAndVariant StreamId StreamVariant

instance eqStreamVariant :: Eq StreamVariant where
  eq = genericEq

instance compareStreamVariant :: Ord StreamVariant where
  compare = genericCompare

instance showStreamVariant :: Show StreamVariant where
  show = genericShow


derive instance genericStreamAndVariant :: Generic StreamAndVariant _

instance eqStreamAndVariant :: Eq StreamAndVariant where
  eq = genericEq

instance compareStreamAndVariant :: Ord StreamAndVariant where
  compare = genericCompare

instance showStreamAndVariant :: Show StreamAndVariant where
  show = genericShow

instance readForeignStreamAndVariant :: ReadForeign StreamAndVariant where
  readImpl fgn = do
                 x <- readString fgn
                 let
                   y = split (Pattern ":") x
                   f = y !! 0 <#> StreamId
                   s = y !! 1 <#> StreamVariant
                   result = lift2 StreamAndVariant f s
                 except $ note (singleton (ForeignError "Failed to parse")) result

instance writeForeignStreamAndVariant :: WriteForeign StreamAndVariant where
  writeImpl (StreamAndVariant streamId streamVariant) = unsafeToForeign $ (unwrap streamId) <> ":" <> (unwrap streamVariant)


data StreamRole = Primary
                | Backup
derive instance eqStreamRole :: Eq StreamRole
derive instance ordStreamRole :: Ord StreamRole


instance readForeignStreamRole :: ReadForeign StreamRole where
  readImpl =
    readString >=> parseAgent
    where
      error s = singleton (ForeignError (errorString s))
      parseAgent s = except $ note (error s) (toType s)
      toType "primary" = pure Primary
      toType "backup" = pure Backup
      toType unknown = Nothing
      errorString s = "Unknown StreamRole: " <> s

instance writeForeignStreamRole :: WriteForeign StreamRole where
  writeImpl =
    toString >>> unsafeToForeign
    where
      toString Primary = "primary"
      toString Backup = "backup"


toStreamId :: StreamAndVariant -> StreamId
toStreamId (StreamAndVariant s _) = s

toVariant :: StreamAndVariant -> StreamVariant
toVariant (StreamAndVariant _ v) = v
