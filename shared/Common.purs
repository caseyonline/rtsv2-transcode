module Shared.Common
       (
         CacheUtilization
       , Milliseconds(..)
       , Url(..)
       , Alert(..)
       , AlertData(..)
       , LoggingContext(..)
       , IngestFailedAlert(..)
       , IngestWarningAlert(..)
       , IngestBitrateType(..)
       , IngestWatermarkType(..)
       , CommonAlertFields
       , LSRSFailedAlert
       , GenericAlert
       , ProfileContext
       , SlotContext
       , LoggingSource
       ) where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Except (except)
import Data.Either (Either(..), note)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.List.NonEmpty (singleton)
import Data.List.NonEmpty as NEL
import Data.Long (Long, toString)
import Data.Long as Long
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, un, wrap)
import Data.Symbol (SProxy(..))
import Foreign (Foreign, ForeignError(..), F, readString, unsafeToForeign)
import Record as Record
import Shared.Rtsv2.Stream (ProfileName, SlotId, SlotName, SlotRole)
import Simple.JSON (class ReadForeign, class WriteForeign, readImpl, writeImpl)

-- TODO - find a place for these utility types to live (a la id3as_common?)
-- | A duration measured in milliseconds.
newtype Milliseconds = Milliseconds Long

-- | Url type
newtype Url = Url String
derive newtype instance showUrl :: Show Url

type CacheUtilization =
  { cacheHits :: Int
  , cacheMisses :: Int
  }

type SlotContext =
  { slotId :: SlotId
  , slotRole :: SlotRole
  , slotName :: Maybe SlotName
  }

type ProfileContext =
  { slotId :: SlotId
  , slotRole :: SlotRole
  , slotName :: SlotName
  , profileName :: ProfileName
  }

data LoggingContext = PerProfile ProfileContext
                    | PerSlot SlotContext

type LoggingSource =
  { "module" :: String
  , "function" :: String
  , line :: Int
  }

data IngestFailedAlert = InvalidVideoCodec Number

data IngestBitrateType = Average
                       | Peak

data IngestWatermarkType = Low
                         | High

data IngestWarningAlert = BitrateExceeded { bitrateType :: IngestBitrateType
                                          , watermarkType :: IngestWatermarkType
                                          , value :: Int
                                          , threshold :: Int }

type LSRSFailedAlert =
  { reason :: String
  }

type GenericAlert =
  { text :: String
  }

data AlertData = IngestStarted
               | IngestFailed IngestFailedAlert
               | IngestWarning IngestWarningAlert
               | LSRSFailed LSRSFailedAlert
               | GenericAlert GenericAlert

type CommonAlertFields a =
  ( initialReport :: Milliseconds
  , lastReport :: Milliseconds
  , repeatCount :: Int
  , context :: Maybe LoggingContext
  , source :: Maybe LoggingSource
  , pid :: String
  | a
  )

newtype Alert = Alert (Record (CommonAlertFields ( alert :: AlertData )))

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
derive instance genericIngestFailedAlert :: Generic IngestFailedAlert _
instance showIngestFailedAlert :: Show IngestFailedAlert where show = genericShow

------------------------------------------------------------------------------
-- IngestWarningAlert
derive instance genericIngestWarningAlert :: Generic IngestWarningAlert _
instance showIngestWarningAlert :: Show IngestWarningAlert where show = genericShow

------------------------------------------------------------------------------
-- IngestWatermarkType
derive instance genericIngestWatermarkType :: Generic IngestWatermarkType _
instance showIngestWatermarkType :: Show IngestWatermarkType where show = genericShow
instance writeForeignIngestWatermarkType :: WriteForeign IngestWatermarkType where
  writeImpl Low = unsafeToForeign "low"
  writeImpl High = unsafeToForeign "high"
instance readForeignIngestWatermarkType :: ReadForeign IngestWatermarkType where
  readImpl = readString >=> parseAgent
    where
      error s = singleton (ForeignError (errorString s))
      parseAgent s = except $ note (error s) (toType s)
      toType "high" = pure High
      toType "low" = pure Low
      toType unknown = Nothing
      errorString s = "Unknown watermark type: " <> s

------------------------------------------------------------------------------
-- IngestBitrateType
derive instance genericIngestBitrateType :: Generic IngestBitrateType _
instance showIngestBitrateType :: Show IngestBitrateType where show = genericShow
instance writeForeignIngestBitrateType :: WriteForeign IngestBitrateType where
  writeImpl Average = unsafeToForeign "average"
  writeImpl Peak = unsafeToForeign "peak"
instance readForeignIngestBitrateType :: ReadForeign IngestBitrateType where
  readImpl = readString >=> parseAgent
    where
      error s = singleton (ForeignError (errorString s))
      parseAgent s = except $ note (error s) (toType s)
      toType "average" = pure Average
      toType "peak" = pure Peak
      toType unknown = Nothing
      errorString s = "Unknown bitrate type: " <> s

------------------------------------------------------------------------------
-- AlertData
derive instance genericAlertData :: Generic AlertData _
instance showAlertData :: Show AlertData where show = genericShow

------------------------------------------------------------------------------
-- LoggingContext
instance writeForeignLoggingContext :: WriteForeign LoggingContext where
  writeImpl (PerProfile profileMetadata) = writeImpl profileMetadata
  writeImpl (PerSlot slotMetadata) = writeImpl slotMetadata

instance readForeignLoggingContext :: ReadForeign LoggingContext where
  readImpl f = (PerProfile <$> readImpl f)
           <|> (PerSlot <$> readImpl f)

derive instance genericLoggingContext :: Generic LoggingContext _
instance showLoggingContext :: Show LoggingContext where show = genericShow

------------------------------------------------------------------------------
-- Alert
derive instance genericAlert :: Generic Alert _
instance showAlert :: Show Alert where show = genericShow

instance writeForeignAlert :: WriteForeign Alert where
  writeImpl (Alert alert) =
    let
      alertDetail :: Record (CommonAlertFields()) -> AlertData -> Foreign
      alertDetail common IngestStarted =
        writeImpl $ Record.merge common { "type" : "ingestStarted"
                                        }

      alertDetail common (IngestFailed (InvalidVideoCodec codecId)) =
        writeImpl $ Record.merge common { "type" : "ingestFailed"
                                        , "reason": "invalidVideoCodec"
                                        , codecId
                                        }

      alertDetail common (IngestWarning (BitrateExceeded {bitrateType, watermarkType, value, threshold})) =
        writeImpl $ Record.merge common { "type" : "ingestWarning"
                                        , "reason": "bitrateExceeded"
                                        , bitrateType
                                        , watermarkType
                                        , value
                                        , threshold
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
     alertDetail (Record.delete (SProxy :: SProxy "alert") alert) alert.alert


instance readForeignAlert :: ReadForeign Alert where
   readImpl f = decode f
     where
       decode o = do
         common <- readImpl o :: F (Record (CommonAlertFields ("type" :: String)))
         let
           alert' = Record.delete (SProxy :: SProxy "type") common

         case common."type" of
           "ingestStarted" ->
             pure $ Alert $ Record.insert (SProxy :: SProxy "alert") IngestStarted alert'

           "ingestFailed" -> do
             {reason} <- readImpl o :: F {reason :: String}
             case reason of
               "invalidVideoCodec" -> do
                 {codecId} <- readImpl o :: F {codecId :: Number}
                 pure $ Alert $ Record.insert (SProxy :: SProxy "alert") (IngestFailed (InvalidVideoCodec codecId)) alert'
               _ ->
                 except $ Left $ singleton (ForeignError ("Unknown ingest failed reason " <> reason))

           "ingestWarning" -> do
             {reason} <- readImpl o :: F {reason :: String}
             case reason of
               "bitrateExceeded" -> do
                 {bitrateType, watermarkType, value, threshold} <- readImpl o :: F { bitrateType :: IngestBitrateType
                                                                                   , watermarkType :: IngestWatermarkType
                                                                                   , value :: Int
                                                                                   , threshold :: Int}
                 pure $ Alert $ Record.insert (SProxy :: SProxy "alert") (IngestWarning (BitrateExceeded { bitrateType
                                                                                                         , watermarkType
                                                                                                         , value
                                                                                                         , threshold })) alert'
               _ ->
                 except $ Left $ singleton (ForeignError ("Unknown ingest warning reason " <> reason))

           "lsrsFailed" -> do
             {reason} <- readImpl o :: F {reason :: String}
             pure $ Alert $ Record.insert (SProxy :: SProxy "alert") (LSRSFailed {reason}) alert'

           "genericAlert" -> do
             {text} <- readImpl o :: F {text :: String}
             pure $ Alert $ Record.insert (SProxy :: SProxy "alert") (GenericAlert {text}) alert'

           _ ->
             except $ Left $ singleton (ForeignError ("Unknown alert type " <> common."type"))

defaultAlert :: Alert
defaultAlert =
  Alert { initialReport : wrap $ Long.fromInt 0
        , lastReport: wrap $ Long.fromInt 0
        , repeatCount: 0
        , context: Nothing
        , source : Nothing
        , pid : ""
        , alert: GenericAlert {text: ""}
        }
