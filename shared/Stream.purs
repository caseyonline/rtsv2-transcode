module Shared.Stream
  ( RtmpShortName(..)
  , RtmpStreamName(..)
  , SlotId(..)
  , SlotIdAndRole(..)
  , SlotRole(..)
  , ProfileName(..)
  , SlotIdAndProfileName(..)
  , SlotNameAndProfileName(..)
  , AgentKey(..)
  , EgestKey(..)
  , AggregatorKey(..)
  , RelayKey(..)
  , IngestKey(..)

  , agentKeyToAggregatorKey
  , aggregatorKeyToAgentKey
  , ingestKeyToAggregatorKey
  , ingestKeyToProfileName

  , toSlotId
  , toProfileName
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
import Simple.JSON (class ReadForeign, class WriteForeign, readImpl, writeImpl)
import Shared.UUID (UUID, fromString)

newtype SlotId = SlotId UUID

data SlotRole = Primary
              | Backup

data SlotIdAndRole = SlotIdAndRole SlotId SlotRole

newtype ProfileName = ProfileName String

data SlotIdAndProfileName = SlotIdAndProfileName SlotId ProfileName

data SlotNameAndProfileName = SlotNameAndProfileName String ProfileName

newtype RtmpShortName = RtmpShortName String

newtype RtmpStreamName = RtmpStreamName String

data EgestKey = EgestKey SlotId SlotRole

data AggregatorKey = AggregatorKey SlotId SlotRole

data RelayKey = RelayKey SlotId SlotRole

data IngestKey = IngestKey SlotId SlotRole ProfileName

data AgentKey = AgentKey SlotId SlotRole

aggregatorKeyToAgentKey :: AggregatorKey -> AgentKey
aggregatorKeyToAgentKey (AggregatorKey slotId slotRole) = AgentKey slotId slotRole

ingestKeyToAggregatorKey :: IngestKey -> AggregatorKey
ingestKeyToAggregatorKey (IngestKey slotId slotRole streamProfileName) = (AggregatorKey slotId slotRole)

agentKeyToAggregatorKey :: AgentKey -> AggregatorKey
agentKeyToAggregatorKey (AgentKey slotId slotRole) = AggregatorKey slotId slotRole

ingestKeyToProfileName :: IngestKey -> ProfileName
ingestKeyToProfileName (IngestKey slotId slotRole streamProfileName) = streamProfileName

toSlotId :: SlotIdAndProfileName -> SlotId
toSlotId (SlotIdAndProfileName s _) = s

toProfileName :: SlotIdAndProfileName -> ProfileName
toProfileName (SlotIdAndProfileName _ v) = v

------------------------------------------------------------------------------
-- Type class derivations
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- RtmpShortName
derive instance genericRtmpShortName :: Generic RtmpShortName _
derive instance newtypeRtmpShortName :: Newtype RtmpShortName _
derive newtype instance readForeignRtmpShortName :: ReadForeign RtmpShortName
derive newtype instance writeForeignRtmpShortName :: WriteForeign RtmpShortName
instance eqRtmpShortName :: Eq RtmpShortName where eq = genericEq
instance compareRtmpShortName :: Ord RtmpShortName where compare = genericCompare
instance showRtmpShortName :: Show RtmpShortName where show = genericShow

------------------------------------------------------------------------------
-- RtmpStreamName
derive instance genericRtmpStreamName :: Generic RtmpStreamName _
derive instance newtypeRtmpStreamName :: Newtype RtmpStreamName _
derive newtype instance readForeignRtmpStreamName :: ReadForeign RtmpStreamName
derive newtype instance writeForeignRtmpStreamName :: WriteForeign RtmpStreamName
instance eqRtmpStreamName :: Eq RtmpStreamName where eq = genericEq
instance compareRtmpStreamName :: Ord RtmpStreamName where compare = genericCompare
instance showRtmpStreamName :: Show RtmpStreamName where show = genericShow

------------------------------------------------------------------------------
-- SlotId
derive instance genericSlotId :: Generic SlotId _
derive instance newtypeSlotId :: Newtype SlotId _
derive newtype instance readForeignSlotId :: ReadForeign SlotId
derive newtype instance writeForeignSlotId :: WriteForeign SlotId

instance eqSlotId :: Eq SlotId where
  eq = genericEq

instance compareSlotId :: Ord SlotId where
  compare = genericCompare

instance showSlotId :: Show SlotId where
  show = genericShow

------------------------------------------------------------------------------
-- SlotIdAndRole
derive instance eqSlotIdAndRole :: Eq SlotIdAndRole
derive instance ordSlotIdAndRole :: Ord SlotIdAndRole

------------------------------------------------------------------------------
-- ProfileName
derive instance genericProfileName :: Generic ProfileName _
derive instance newtypeProfileName :: Newtype ProfileName _
derive newtype instance readForeignProfileName :: ReadForeign ProfileName
derive newtype instance writeForeignProfileName :: WriteForeign ProfileName
instance eqProfileName :: Eq ProfileName where
  eq = genericEq

instance compareProfileName :: Ord ProfileName where
  compare = genericCompare

instance showProfileName :: Show ProfileName where
  show = genericShow

------------------------------------------------------------------------------
-- SlotIdAndProfileName
derive instance genericSlotIdAndProfileName :: Generic SlotIdAndProfileName _

instance eqSlotIdAndProfileName :: Eq SlotIdAndProfileName where
  eq = genericEq

instance compareSlotIdAndProfileName :: Ord SlotIdAndProfileName where
  compare = genericCompare

instance showSlotIdAndProfileName :: Show SlotIdAndProfileName where
  show = genericShow

instance readForeignSlotIdAndProfileName :: ReadForeign SlotIdAndProfileName where
  readImpl fgn = do
                 x <- readString fgn
                 let
                   y = split (Pattern ":") x
                   f = y !! 0 >>= fromString <#> SlotId
                   s = y !! 1 <#> ProfileName
                   result = lift2 SlotIdAndProfileName f s
                 except $ note (singleton (ForeignError "Failed to parse")) result

instance writeForeignSlotIdAndProfileName :: WriteForeign SlotIdAndProfileName where
  writeImpl (SlotIdAndProfileName slotId profileName) = unsafeToForeign $ (show (unwrap slotId)) <> ":" <> (unwrap profileName)

------------------------------------------------------------------------------
-- SlotNameAndProfileName
derive instance genericSlotNameAndProfileName :: Generic SlotNameAndProfileName _

instance eqSlotNameAndProfileName :: Eq SlotNameAndProfileName where
  eq = genericEq

instance compareSlotNameAndProfileName :: Ord SlotNameAndProfileName where
  compare = genericCompare

instance showSlotNameAndProfileName :: Show SlotNameAndProfileName where
  show = genericShow

instance readForeignSlotNameAndProfileName :: ReadForeign SlotNameAndProfileName where
  readImpl fgn = do
                 x <- readString fgn
                 let
                   y = split (Pattern ":") x
                   f = y !! 0
                   s = y !! 1 <#> ProfileName
                   result = lift2 SlotNameAndProfileName f s
                 except $ note (singleton (ForeignError "Failed to parse")) result

instance writeForeignSlotNameAndProfileName :: WriteForeign SlotNameAndProfileName where
  writeImpl (SlotNameAndProfileName slotName profileName) = unsafeToForeign $ slotName <> ":" <> (unwrap profileName)

------------------------------------------------------------------------------
-- SlotRole
derive instance eqSlotRole :: Eq SlotRole
derive instance ordSlotRole :: Ord SlotRole
instance showSlotRole :: Show SlotRole where
  show Primary = "primary"
  show Backup = "backup"
instance readForeignSlotRole :: ReadForeign SlotRole where
  readImpl =
    readString >=> parseAgent
    where
      error s = singleton (ForeignError (errorString s))
      parseAgent s = except $ note (error s) (toType s)
      toType "primary" = pure Primary
      toType "backup" = pure Backup
      toType unknown = Nothing
      errorString s = "Unknown SlotRole: " <> s
instance writeForeignSlotRole :: WriteForeign SlotRole where
  writeImpl =
    toString >>> unsafeToForeign
    where
      toString Primary = "primary"
      toString Backup = "backup"


------------------------------------------------------------------------------
-- EgestKey
derive instance eqEgestKey :: Eq EgestKey
derive instance ordEgestKey :: Ord EgestKey

------------------------------------------------------------------------------
-- AggregatorKey
derive instance eqAggregatorKey :: Eq AggregatorKey
derive instance ordAggregatorKey :: Ord AggregatorKey

------------------------------------------------------------------------------
-- RelayKey
derive instance eqRelayKey :: Eq RelayKey
derive instance ordRelayKey :: Ord RelayKey

------------------------------------------------------------------------------
-- IngestKey
derive instance eqIngestKey :: Eq IngestKey
derive instance ordIngestKey :: Ord IngestKey

type IngestKeyJson = { slotId :: SlotId
                     , role :: SlotRole
                     , profile :: ProfileName
                     }

instance readForeignIngestKey :: ReadForeign IngestKey where
  readImpl f =
    mapper <$> readImpl f
    where
      mapper :: IngestKeyJson -> IngestKey
      mapper {slotId, role: slotRole, profile: streamProfileName} = IngestKey slotId slotRole streamProfileName

instance writeForeignIngestKey :: WriteForeign IngestKey where
  writeImpl (IngestKey slotId slotRole streamProfileName) = writeImpl { slotId: slotId
                                                                      , role: slotRole
                                                                      , profile: streamProfileName}
------------------------------------------------------------------------------
-- AgentKey
derive instance eqAgentKey :: Eq AgentKey
derive instance ordAgentKey :: Ord AgentKey
