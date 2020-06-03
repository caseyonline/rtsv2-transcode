module Shared.Common
       (
         Milliseconds(..)
       , Url(..)
       , Alert(..)
       , AlertData(..)
       , LoggingMetadata(..)
       , IngestFailedAlert(..)
       , LSRSFailedAlert
       , GenericAlert
       , ProfileMetadata
       , SlotMetadata
       , LoggingSource
       ) where

import Prelude

import Control.Monad.Except (except)
import Data.Either (note)
import Data.List.NonEmpty as NEL
import Data.Long (Long, toString)
import Data.Long as Long
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, un)
import Foreign (ForeignError(..), readString, unsafeToForeign)
import Record as Record
import Shared.Rtsv2.Stream (ProfileName, SlotId, SlotRole)
import Simple.JSON (class ReadForeign, class WriteForeign, writeImpl)

-- TODO - find a place for these utility types to live (a la id3as_common?)
-- | A duration measured in milliseconds.
newtype Milliseconds = Milliseconds Long

-- | Url type
newtype Url = Url String
derive newtype instance showUrl :: Show Url


type SlotMetadata =
  { slotId :: SlotId
  , slotRole :: SlotRole
  , slotName :: Maybe String
  }

type ProfileMetadata =
  { slotId :: SlotId
  , slotRole :: SlotRole
  , slotName :: String
  , profileName :: ProfileName
  }

data LoggingMetadata = PerProfile ProfileMetadata
                     | PerSlot SlotMetadata

type LoggingSource =
  { "module" :: String
  , "function" :: String
  , line :: Int
  }

data IngestFailedAlert = InvalidVideoCodec Number

type LSRSFailedAlert =
  { reason :: String
  }

type GenericAlert =
  { text :: String
  }

data AlertData = IngestStarted
               | IngestFailed IngestFailedAlert
               | LSRSFailed LSRSFailedAlert
               | GenericAlert GenericAlert

newtype Alert = Alert { initialReport :: Milliseconds
                      , lastReport :: Milliseconds
                      , repeatCount :: Int
                      , alert :: AlertData
                      , metadata :: Maybe LoggingMetadata
                      , source :: Maybe LoggingSource
                      , pid :: String
                      }

------------------------------------------------------------------------------
-- Type class derivations
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- Milliseconds
derive instance newtypeMilliseconds :: Newtype Milliseconds _
derive newtype instance eqMilliseconds :: Eq Milliseconds
derive newtype instance ordMilliseconds :: Ord Milliseconds
instance readForeignMilliseconds :: ReadForeign Milliseconds where
  readImpl = readString >=> parseMillis
    where
      error s = NEL.singleton (ForeignError (errorString s))
      errorString s = "Couldn't parse long: " <> s
      parseMillis s = except $ note (error s) $ Milliseconds <$> Long.fromString s

instance writeForeignMilliseconds :: WriteForeign Milliseconds where
  writeImpl = unsafeToForeign <<< toString <<< un Milliseconds

instance semigroupMilliseconds :: Semigroup Milliseconds where
  append (Milliseconds x) (Milliseconds y) = Milliseconds (x + y)

derive newtype instance semiringMilliseconds :: Semiring Milliseconds

instance ringMilliseconds :: Ring Milliseconds where
  sub (Milliseconds x) (Milliseconds y) = Milliseconds (x - y)

instance monoidMilliseconds :: Monoid Milliseconds where
  mempty = Milliseconds $ Long.fromInt 0

instance showMilliseconds :: Show Milliseconds where
  show (Milliseconds n) = "(Milliseconds " <> show n <> ")"

------------------------------------------------------------------------------
-- Url
derive instance newtypeURL :: Newtype Url _
derive newtype instance eqURL :: Eq Url
derive newtype instance ordURL :: Ord Url
derive newtype instance readForeignUrl :: ReadForeign Url
derive newtype instance writeForeignUrl :: WriteForeign Url

------------------------------------------------------------------------------
-- IngestFailedAlert
instance writeForeignIngestFailedAlert :: WriteForeign IngestFailedAlert where
  writeImpl (InvalidVideoCodec codecId) = writeImpl {invalidVideoCodec: codecId}

------------------------------------------------------------------------------
-- LoggingMetadata
instance writeForeignLoggingMetadata :: WriteForeign LoggingMetadata where
  writeImpl (PerProfile profileMetadata) = writeImpl profileMetadata
  writeImpl (PerSlot slotMetadata) = writeImpl slotMetadata

------------------------------------------------------------------------------
-- Alert
instance writeForeignAlert :: WriteForeign Alert where
  writeImpl (Alert alert) =
    let
      alertCommon {initialReport, lastReport, repeatCount, metadata, source, pid} =
        {initialReport, lastReport, repeatCount, metadata, source, pid}

      alertDetail common IngestStarted =
        writeImpl $ Record.merge common { "type" : "ingestStarted"
                                        }

      alertDetail common (IngestFailed (InvalidVideoCodec codecId)) =
        writeImpl $ Record.merge common { "type" : "ingestFailed"
                                        , "reason": "invalidVideoCodec"
                                        , codecId
                                        }

      alertDetail common (LSRSFailed {reason}) =
        writeImpl $ Record.merge common { "type" : "lsrsFailed"
                                        , reason
                                        }

      alertDetail common (GenericAlert {text}) =
        writeImpl $ Record.merge common { "type" : "genericAlert"
                                        , text
                                        }
    in
     alertDetail (alertCommon alert) alert.alert
