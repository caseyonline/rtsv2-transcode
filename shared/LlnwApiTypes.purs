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
       , StreamPublish(..)
       , StreamDetails
       , SlotDetails
       , StreamOutputFormat
       , HlsPushSpec
       , SlotProfile(..)
       , HlsPushSpecFormat
       , HlsPushAuth
       , SlotLookupResult
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
import Data.Newtype (class Newtype)
import Foreign (ForeignError(..), readString, unsafeToForeign)
import Record (rename)
import Shared.Stream (ProfileName, RtmpShortName, RtmpStreamName, SlotId, SlotRole)
import Simple.JSON (class ReadForeign, class WriteForeign, readImpl, writeImpl)
import Type.Prelude (SProxy(..))

data StreamIngestProtocol = Rtmp
                          | WebRTC

newtype StreamConnection = StreamConnection
                           { host :: String
                           , protocol :: StreamIngestProtocol
                           , rtmpShortName :: RtmpShortName
                           }

data SlotPublishAuthType = Adobe
                         | Llnw
                         | Query

type AuthType =
  { authType :: SlotPublishAuthType
  }

newtype StreamAuth = StreamAuth
                     { host :: String
                     , rtmpShortName :: RtmpShortName
                     , username :: String
                     }

newtype PublishCredentials = PublishCredentials
                             { username :: String
                             , password :: String
                             }

newtype StreamPublish = StreamPublish
                        { host :: String
                        , protocol :: StreamIngestProtocol
                        , rtmpShortName :: RtmpShortName
                        , rtmpStreamName :: RtmpStreamName
                        , username :: String
                        }

newtype SlotProfile = SlotProfile
                      { name :: ProfileName
                      , rtmpStreamName :: RtmpStreamName
                      , bitrate :: Int
                      }

type SlotDetails =
  { id :: SlotId
  , name :: String
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
  { role :: SlotRole
  , slot :: SlotDetails
  , push :: Array HlsPushSpec
  }

type SlotLookupResult =
  { id :: SlotId
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

derive instance newtypePublishCredentials :: Newtype PublishCredentials _
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

derive instance eqHlsPushSpecFormat :: Eq HlsPushSpecFormat
instance showHlsPushSpecFormat :: Show HlsPushSpecFormat where
  show Hls = "HLS"

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

derive instance eqStreamOutputFormat :: Eq StreamOutputFormat
instance showStreamOutputFormat :: Show StreamOutputFormat where
  show WebRTCOutput = "WebRtc"
  show RtmpOutput = "RTMP"
  show HlsOutput = "HLS"

derive instance newtypeStreamAuth :: Newtype StreamAuth _
derive instance genericStreamAuth :: Generic StreamAuth _
instance eqStreamAuth :: Eq StreamAuth where eq = genericEq
instance compareStreamAuth :: Ord StreamAuth where compare = genericCompare

instance readForeignStreamAuth :: ReadForeign StreamAuth where
  readImpl o = decode o
    where
    decode s = do
      parsed <- readImpl s
      pure
        $ StreamAuth
        $ rename (SProxy :: SProxy "shortname") (SProxy :: SProxy "rtmpShortName") parsed

instance writeForeignStreamAuth :: WriteForeign StreamAuth where
  writeImpl = (unsafeToForeign <<< encode)
    where
      encode (StreamAuth r) = do
        writeImpl
          $ rename (SProxy :: SProxy "rtmpShortName") (SProxy :: SProxy "shortname") r


derive instance newtypeStreamConnection :: Newtype StreamConnection _
derive instance genericStreamConnection :: Generic StreamConnection _
instance eqStreamConnection :: Eq StreamConnection where eq = genericEq
instance compareStreamConnection :: Ord StreamConnection where compare = genericCompare

instance readForeignStreamConnection :: ReadForeign StreamConnection where
  readImpl o = decode o
    where
    decode s = do
      parsed <- readImpl o
      pure
        $ StreamConnection
        $ rename (SProxy :: SProxy "shortname") (SProxy :: SProxy "rtmpShortName") parsed

instance writeForeignStreamConnection :: WriteForeign StreamConnection where
  writeImpl = (unsafeToForeign <<< encode)
    where
      encode (StreamConnection r) = do
        writeImpl
          $ rename (SProxy :: SProxy "rtmpShortName") (SProxy :: SProxy "shortname") r


derive instance newtypeSlotProfile :: Newtype SlotProfile _
derive instance genericSlotProfile :: Generic SlotProfile _
instance eqSlotProfile :: Eq SlotProfile where eq = genericEq
instance compareSlotProfile :: Ord SlotProfile where compare = genericCompare
instance showSlotProfile :: Show SlotProfile where show = genericShow

instance readForeignSlotProfile :: ReadForeign SlotProfile where
  readImpl o = decode o
    where
    decode s = do
      parsed <- readImpl s
      pure
        $ SlotProfile
        $ rename (SProxy :: SProxy "streamName") (SProxy :: SProxy "rtmpStreamName") parsed

instance writeForeignSlotProfile :: WriteForeign SlotProfile where
  writeImpl = (unsafeToForeign <<< encode)
    where
      encode (SlotProfile r) = do
        writeImpl $
          rename (SProxy :: SProxy "rtmpStreamName") (SProxy :: SProxy "streamName") r

derive instance newtypeStreamPublish :: Newtype StreamPublish _
derive instance genericStreamPublish :: Generic StreamPublish _
instance eqStreamPublish :: Eq StreamPublish where eq = genericEq
instance compareStreamPublish :: Ord StreamPublish where compare = genericCompare

instance readForeignStreamPublish :: ReadForeign StreamPublish where
  readImpl o = decode o
    where
    decode s = do
      parsed <- readImpl s
      pure
        $ StreamPublish
        $ rename (SProxy :: SProxy "shortname") (SProxy :: SProxy "rtmpShortName")
        $ rename (SProxy :: SProxy "streamName") (SProxy :: SProxy "rtmpStreamName") parsed

instance writeForeignStreamPublish :: WriteForeign StreamPublish where
  writeImpl = (unsafeToForeign <<< encode)
    where
      encode (StreamPublish r) = do
        writeImpl
          $ rename (SProxy :: SProxy "rtmpShortName") (SProxy :: SProxy "shortname")
          $ rename (SProxy :: SProxy "rtmpStreamName") (SProxy :: SProxy "streamName") r
