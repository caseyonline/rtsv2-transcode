module Shared.Types.Media.Types.Rtmp
       ( RtmpClientMetadata
       , RtmpClientAudioMetadata
       , RtmpClientVideoMetadata
       , RtmpClientMetadataValue
       , RtmpClientMetadataNamedValue
       , RtmpAudioCodecId(..)
       , RtmpVideoCodecId(..)
       , emptyRtmpClientMetadata
       ) where

import Prelude

import Control.Monad.Except (except)
import Data.Either (note)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.List.NonEmpty (singleton)
import Data.Maybe (Maybe(..))
import Foreign (ForeignError(..), readString, unsafeToForeign)
import Shared.Types.Media.Types.Avc (AvcProfile, AvcLevel)
import Simple.JSON (class ReadForeign, class WriteForeign, writeImpl)
import Simple.JSON.Generics (untaggedSumRep)

------------------------------------------------------------------------------
-- RTMP Client Metadata - if you change these, then you need to change Rtmp.erl
------------------------------------------------------------------------------
data RtmpAudioCodecId = Uncompressed
                        | ADPCM
                        | MP3
                        | Nellymoser8Mono
                        | Nellymoser
                        | AAC
                        | Speex

data RtmpVideoCodecId = SorensonH263
                        | Screen
                        | On2VP6
                        | On2VP6Transparency
                        | H264

data RtmpClientMetadataValue = RtmpBool Boolean
                             | RtmpString String
                             | RtmpInt Int
                             | RtmpFloat Number

type RtmpClientMetadataNamedValue = { name :: String
                                    , value :: RtmpClientMetadataValue}

type RtmpClientAudioMetadata =
  { codecId :: Maybe RtmpAudioCodecId
  , dataRate :: Maybe Number
  , sampleRate :: Maybe Int
  , sampleSize :: Maybe Int
  , stereo :: Maybe Boolean
  , device :: Maybe String
  , channels :: Maybe Int
  , inputVolume :: Maybe Number
  }

type RtmpClientVideoMetadata =
  { codecId :: Maybe RtmpVideoCodecId
  , dataRate :: Maybe Number
  , frameRate :: Maybe Number
  , width :: Maybe Int
  , height :: Maybe Int
  , device :: Maybe String
  , keyFrameFrequency :: Maybe Number
  , avcProfile :: Maybe AvcProfile
  , avcLevel :: Maybe AvcLevel
  }

type RtmpClientMetadata f =
  { audio :: RtmpClientAudioMetadata
  , video :: RtmpClientVideoMetadata
  , duration :: Maybe Number
  , encoder :: Maybe String
  , filesize :: Maybe Number
  , other :: f RtmpClientMetadataNamedValue
  }

emptyRtmpClientMetadata :: forall f. Monoid (f RtmpClientMetadataNamedValue) => (RtmpClientMetadata f)
emptyRtmpClientMetadata = { audio: { codecId: Nothing
                                   , dataRate: Nothing
                                   , sampleRate: Nothing
                                   , sampleSize: Nothing
                                   , stereo: Nothing
                                   , device: Nothing
                                   , channels: Nothing
                                   , inputVolume: Nothing
                                   }
                          , video: { codecId: Nothing
                                   , dataRate: Nothing
                                   , frameRate: Nothing
                                   , width: Nothing
                                   , height: Nothing
                                   , device: Nothing
                                   , keyFrameFrequency: Nothing
                                   , avcLevel: Nothing
                                   , avcProfile: Nothing
                                   }
                          , duration: Nothing
                          , encoder: Nothing
                          , filesize: Nothing
                          , other: mempty
                          }

--------------------------------------------------------------------------------
-- Type class derivations
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- RtmpAudioCodecId
derive instance genericRtmpAudioCodecId :: Generic RtmpAudioCodecId _
derive instance eqRtmpAudioCodecId :: Eq RtmpAudioCodecId
instance showRtmpAudioCodecId :: Show RtmpAudioCodecId where show = genericShow
instance readRtmpAudioCodecId :: ReadForeign RtmpAudioCodecId where
  readImpl =
    readString >=> parseStr
    where
      error s = singleton (ForeignError (errorString s))
      parseStr s = except $ note (error s) (toType s)
      toType "uncompressed" = pure Uncompressed
      toType "adpcm" = pure ADPCM
      toType "mp3" = pure MP3
      toType "nellymoser8" = pure Nellymoser8Mono
      toType "nellymoser" = pure Nellymoser
      toType "aac" = pure AAC
      toType "speex" = pure Speex
      toType unknown = Nothing
      errorString s = "Unknown RtmpAudioCodecId: " <> s
instance writeForeignRtmpAudioCodecId :: WriteForeign RtmpAudioCodecId where
  writeImpl =
    toString >>> unsafeToForeign
    where
      toString Uncompressed = "uncompressed"
      toString ADPCM = "adpcm"
      toString MP3 = "mp3"
      toString Nellymoser8Mono = "nellymoser8"
      toString Nellymoser = "nellymoser"
      toString AAC = "aac"
      toString Speex = "speex"

--------------------------------------------------------------------------------
-- RtmpVideoCodecId
derive instance genericRtmpVideoCodecId :: Generic RtmpVideoCodecId _
derive instance eqRtmpVideoCodecId :: Eq RtmpVideoCodecId
instance showRtmpVideoCodecId :: Show RtmpVideoCodecId where show = genericShow
instance readRtmpVideoCodecId :: ReadForeign RtmpVideoCodecId where
  readImpl =
    readString >=> parseStr
    where
      error s = singleton (ForeignError (errorString s))
      parseStr s = except $ note (error s) (toType s)
      toType "sorenson" = pure SorensonH263
      toType "screen" = pure Screen
      toType "on2vp6" = pure On2VP6
      toType "on2vp6transparency" = pure On2VP6Transparency
      toType "h264" = pure H264
      toType unknown = Nothing
      errorString s = "Unknown RtmpVideoCodecId: " <> s
instance writeForeignRtmpVideoCodecId :: WriteForeign RtmpVideoCodecId where
  writeImpl =
    toString >>> unsafeToForeign
    where
      toString SorensonH263 = "sorenson"
      toString Screen = "screen"
      toString On2VP6 = "on2vp6"
      toString On2VP6Transparency = "on2vp6transparency"
      toString H264 = "h264"

--------------------------------------------------------------------------------
-- RtmpClientMetadataValue
derive instance genericRtmpClientMetadataValue :: Generic RtmpClientMetadataValue _
derive instance eqRtmpClientMetadataValue :: Eq RtmpClientMetadataValue
instance showRtmpClientMetadataValue :: Show RtmpClientMetadataValue where show = genericShow
instance readRtmpClientMetadataValue :: ReadForeign RtmpClientMetadataValue where readImpl = untaggedSumRep
instance writeForeignRtmpClientMetadataValue :: WriteForeign RtmpClientMetadataValue where
  writeImpl (RtmpBool bool) = writeImpl bool
  writeImpl (RtmpString string) = writeImpl string
  writeImpl (RtmpInt int) = writeImpl int
  writeImpl (RtmpFloat float) = writeImpl float
