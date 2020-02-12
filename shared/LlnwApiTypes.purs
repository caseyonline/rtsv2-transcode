-- Notes:
-- * Unique ID per slot (64 bit int?)
-- * What does 415 error indicate?
-- * is 'profiles' within SlotDetails guaranteed to be non-empty?
-- * Capitization differs on shortname vs streamName
-- * Can slot details change when stream is live - assuming not

module Shared.LlnwApiTypes
       ( StreamConnection
       , AuthType
       , StreamIngestProtocol(..)
       , SlotPublishAuthType(..)
       , StreamAuth
       , PublishCredentials(..)
       , StreamPublish
       , StreamDetails
       , SlotDetails
       , StreamOutputFormat
       , HlsPushSpec
       , SlotProfile
       , HlsPushSpecFormat
       , HlsPushAuth
       )
       where

import Prelude

import Control.Monad.Except (except)
import Data.Either (note)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Ord (genericCompare)
import Data.Generic.Rep.Show (genericShow)
import Data.List.NonEmpty (singleton)
import Data.Maybe (Maybe(..))
import Foreign (ForeignError(..), readString, unsafeToForeign)
import Shared.Stream (StreamRole)
import Simple.JSON (class ReadForeign, class WriteForeign)

data StreamIngestProtocol = Rtmp
                          | WebRTC

type StreamConnection =
  { host :: String
  , protocol :: StreamIngestProtocol
  , shortname :: String
  }

data SlotPublishAuthType = Adobe
                         | Llnw
                         | Query

type AuthType =
  { authType :: SlotPublishAuthType
  }

type StreamAuth =
  { host :: String
  , shortname :: String
  , username :: String
  }

newtype PublishCredentials = PublishCredentials
                             { username :: String
                             , password :: String
                             }

type StreamPublish =
  { host :: String
  , protocol :: StreamIngestProtocol
  , shortname :: String
  , streamName :: String
  , username :: String
  }

type SlotProfile =
  { name :: String
  , streamName :: String
  , bitrate :: Int
  }

type SlotDetails =
  { name :: String
  , subscribeValidation :: Boolean
  , outputFormats :: Array StreamOutputFormat
  , profiles :: Array SlotProfile
  }

type HlsPushAuth =
  { username :: String
  , password :: String
  }

data HlsPushSpecFormat = Hls

type HlsPushSpec =
  { format :: HlsPushSpecFormat
  , baseUrl :: String
  , auth :: HlsPushAuth
  }

data StreamOutputFormat = WebRTCOutput
                        | RtmpOutput
                        | HlsOutput

type StreamDetails =
  { role :: StreamRole
  , slot :: SlotDetails
  , push :: Array HlsPushSpec
  }

--------------------------------------------------------------------------------
-- Type class derivations
--------------------------------------------------------------------------------
derive instance genericStreamIngestProtocol :: Generic StreamIngestProtocol _

instance readForeignStreamIngestProtocol :: ReadForeign StreamIngestProtocol where
  readImpl =
    readString >=> parseAgent
    where
      error s = singleton (ForeignError (errorString s))
      parseAgent s = except $ note (error s) (toType s)
      toType "rtmp" = pure Rtmp
      toType "webrtc" = pure WebRTC
      toType unknown = Nothing
      errorString s = "Unknown StreamIngestProtocol: " <> s

instance writeForeignStreamIngestProtocol :: WriteForeign StreamIngestProtocol where
  writeImpl =
    toString >>> unsafeToForeign
    where
      toString Rtmp = "rtmp"
      toString WebRTC = "webrtc"

instance eqStreamIngestProtocol :: Eq StreamIngestProtocol where
  eq = genericEq

instance compareStreamIngestProtocol :: Ord StreamIngestProtocol where
  compare = genericCompare

instance showStreamIngestProtocol :: Show StreamIngestProtocol where
  show = genericShow

derive newtype instance readForeignPublishCredentials :: ReadForeign PublishCredentials
derive newtype instance writeForeignPublishCredentials :: WriteForeign PublishCredentials

derive instance genericSlotPublishAuthType :: Generic SlotPublishAuthType _

instance readForeignSlotPublishAuthType :: ReadForeign SlotPublishAuthType where
  readImpl =
    readString >=> parseAgent
    where
      error s = singleton (ForeignError (errorString s))
      parseAgent s = except $ note (error s) (toType s)
      toType "adobe" = pure Adobe
      toType "llnw" = pure Llnw
      toType "query" = pure Query
      toType unknown = Nothing
      errorString s = "Unknown SlotPublishAuthType: " <> s

instance writeForeignSlotPublishAuthType :: WriteForeign SlotPublishAuthType where
  writeImpl =
    toString >>> unsafeToForeign
    where
      toString Adobe = "adobe"
      toString Llnw = "llnw"
      toString Query = "query"

instance eqSlotPublishAuthType :: Eq SlotPublishAuthType where
  eq = genericEq

instance compareSlotPublishAuthType :: Ord SlotPublishAuthType where
  compare = genericCompare

instance showSlotPublishAuthType :: Show SlotPublishAuthType where
  show = genericShow

instance readForeignHlsPushSpecFormat :: ReadForeign HlsPushSpecFormat where
  readImpl =
    readString >=> parseAgent
    where
      error s = singleton (ForeignError (errorString s))
      parseAgent s = except $ note (error s) (toType s)
      toType "hls" = pure Hls
      toType unknown = Nothing
      errorString s = "Unknown HlsPushSpecFormat: " <> s

instance writeForeignHlsPushSpecFormat :: WriteForeign HlsPushSpecFormat where
  writeImpl =
    toString >>> unsafeToForeign
    where
      toString Hls = "hls"

instance readForeignStreamOutputFormat :: ReadForeign StreamOutputFormat where
  readImpl =
    readString >=> parseAgent
    where
      error s = singleton (ForeignError (errorString s))
      parseAgent s = except $ note (error s) (toType s)
      toType "webrtc" = pure WebRTCOutput
      toType "rtmp" = pure RtmpOutput
      toType "hls" = pure HlsOutput
      toType unknown = Nothing
      errorString s = "Unknown StreamOutputFormat: " <> s

instance writeForeignStreamOutputFormat :: WriteForeign StreamOutputFormat where
  writeImpl =
    toString >>> unsafeToForeign
    where
      toString WebRTCOutput = "webrtc"
      toString RtmpOutput = "rtmp"
      toString HlsOutput = "hls"
