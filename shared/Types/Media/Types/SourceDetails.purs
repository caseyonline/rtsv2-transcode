module Shared.Types.Media.Types.SourceDetails
       ( StreamId
       , AudioCodec(..)
       , VideoCodec(..)
       , Width
       , Height
       , PixelAspectRatio
       , SampleRate
       , ChannelLayout
       , AudioStreamInfo
       , VideoStreamInfo
       , SourceInfo
       ) where

import Prelude

import Control.Apply (lift2)
import Control.Monad.Except (except)
import Data.Array ((!!))
import Data.Either (note)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.List.NonEmpty (singleton)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Tuple (Tuple(..))
import Foreign (F, Foreign, ForeignError(..), readArray, readInt, readString, unsafeToForeign)
import Logger (spy)
import Simple.JSON (class ReadForeign, class WriteForeign, readImpl, writeImpl)

newtype StreamId = StreamId Int

data AudioCodec = RawAudio
                | ADPCM
                | MP2
                | MP3
                | LinearPCM
                | Nellymoser
                | ALaw
                | ULaw
                | AAC
                | Speex
                | AC3
                | EAC3
                | G722
                | Opus

data VideoCodec = RawVideo
                | VC1
                | H263
                | H264
                | H265
                | MPEG2
                | JPEG
                | JPEG2000

newtype Width = Width Int

newtype Height = Height Int

newtype SampleRate = SampleRate Int

newtype PixelAspectRatio = PixelAspectRatio (Tuple Width Height)

data ChannelLayout = Mono
                   | Stereo


type VideoStreamInfo =
  { streamId :: StreamId
  , codec :: Maybe VideoCodec
  , width :: Maybe Width
  , height :: Maybe Height
  , pixelAspectRatio :: Maybe PixelAspectRatio
  , interlaced :: Maybe Boolean
  , resolutionName :: Maybe String
  , frameRateName :: Maybe String
  , language :: Maybe String
  , inFillinMode :: Boolean
  }

type AudioStreamInfo =
  { streamId :: StreamId
  , codec :: Maybe AudioCodec
  , sampleRate :: Maybe SampleRate
  , channelLayout :: Maybe ChannelLayout
  , language :: Maybe String
  , inFillinMode :: Boolean
  , dvbStreamId :: Maybe Int
  }

type SourceInfo f =
  { audioStreams :: f AudioStreamInfo
  , videoStreams :: f VideoStreamInfo
  }

--------------------------------------------------------------------------------
-- Type class derivations
--------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- ChannelLayout
derive instance genericChannelLayout :: Generic ChannelLayout _
derive instance eqChannelLayout :: Eq ChannelLayout
instance showChannelLayout :: Show ChannelLayout where show = genericShow
instance readChannelLayout :: ReadForeign ChannelLayout where
  readImpl =
    readString >=> parseStr
    where
      error s = singleton (ForeignError (errorString s))
      parseStr s = except $ note (error s) (toType s)
      toType "mono" = pure Mono
      toType "stereo" = pure Stereo
      toType unknown = Nothing
      errorString s = "Unknown ChannelLayout: " <> s
instance writeForeignChannelLayout :: WriteForeign ChannelLayout where
  writeImpl =
    toString >>> unsafeToForeign
    where
      toString Mono = "mono"
      toString Stereo = "stereo"

------------------------------------------------------------------------------
-- AudioCodec
derive instance genericAudioCodec :: Generic AudioCodec _
derive instance eqAudioCodec :: Eq AudioCodec
instance showAudioCodec :: Show AudioCodec where show = genericShow
instance readAudioCodec :: ReadForeign AudioCodec where
  readImpl =
    readString >=> parseStr
    where
      error s = singleton (ForeignError (errorString s))
      parseStr s = except $ note (error s) (toType s)
      toType "raw" = pure RawAudio
      toType "adpcm" = pure ADPCM
      toType "mp2" = pure MP2
      toType "mp3" = pure MP3
      toType "linear_pcm" = pure LinearPCM
      toType "nellymoser" = pure Nellymoser
      toType "alaw" = pure ALaw
      toType "ulaw" = pure ULaw
      toType "aac" = pure AAC
      toType "speex" = pure Speex
      toType "ac3" = pure AC3
      toType "eac3" = pure EAC3
      toType "opus" = pure Opus
      toType "g722" = pure G722
      toType unknown = Nothing
      errorString s = "Unknown AudioCodec: " <> s
instance writeForeignAudioCodec :: WriteForeign AudioCodec where
  writeImpl =
    toString >>> unsafeToForeign
    where
      toString RawAudio = "raw"
      toString ADPCM = "adpcm"
      toString MP2 = "mp2"
      toString MP3 = "mp3"
      toString LinearPCM = "linear_pcm"
      toString Nellymoser = "nellymoser"
      toString ALaw = "alaw"
      toString ULaw = "ulaw"
      toString AAC = "aac"
      toString Speex = "speex"
      toString AC3 = "ac3"
      toString EAC3 = "eac3"
      toString Opus = "opus"
      toString G722 = "g722"

------------------------------------------------------------------------------
-- VideoCodec
derive instance genericVideoCodec :: Generic VideoCodec _
derive instance eqVideoCodec :: Eq VideoCodec
instance showVideoCodec :: Show VideoCodec where show = genericShow
instance readVideoCodec :: ReadForeign VideoCodec where
  readImpl =
    readString >=> parseStr
    where
      error s = singleton (ForeignError (errorString s))
      parseStr s = except $ note (error s) (toType s)
      toType "raw" = pure RawVideo
      toType "vc1" = pure VC1
      toType "h263" = pure H263
      toType "h264" = pure H264
      toType "h265" = pure H265
      toType "mpeg2" = pure MPEG2
      toType "jpeg" = pure JPEG
      toType "jpeg2000" = pure JPEG2000
      toType unknown = Nothing
      errorString s = "Unknown VideoCodec: " <> s
instance writeForeignVideoCodec :: WriteForeign VideoCodec where
  writeImpl =
    toString >>> unsafeToForeign
    where
      toString RawVideo = "raw"
      toString VC1 = "vc1"
      toString H263 = "h263"
      toString H264 = "h264"
      toString H265 = "h265"
      toString MPEG2 = "mpeg2"
      toString JPEG = "jpeg"
      toString JPEG2000 = "jpeg2000"

------------------------------------------------------------------------------
-- StreamId
derive instance newtypeStreamId :: Newtype StreamId _
derive newtype instance readForeignStreamId :: ReadForeign StreamId
derive newtype instance writeForeignStreamId :: WriteForeign StreamId

------------------------------------------------------------------------------
-- SampleRate
derive instance newtypeSampleRate :: Newtype SampleRate _
derive newtype instance readForeignSampleRate :: ReadForeign SampleRate
derive newtype instance writeForeignSampleRate :: WriteForeign SampleRate

------------------------------------------------------------------------------
-- Width
derive instance newtypeWidth :: Newtype Width _
derive newtype instance readForeignWidth :: ReadForeign Width
derive newtype instance writeForeignWidth :: WriteForeign Width

------------------------------------------------------------------------------
-- Height
derive instance newtypeHeight :: Newtype Height _
derive newtype instance readForeignHeight :: ReadForeign Height
derive newtype instance writeForeignHeight :: WriteForeign Height

------------------------------------------------------------------------------
-- PixelAspectRatio
instance readForeignPixelAspectRatio :: ReadForeign PixelAspectRatio where
  readImpl fgn = do
    x <- readImpl fgn
    let
     error :: forall a. Maybe a -> F a
     error m = except $ note (singleton (ForeignError "Failed to parse")) m

     readIndex :: Int -> Array Foreign -> F Int
     readIndex index array = readInt =<< (error $ array !! index)

     fst = wrap <$> readIndex 0 x
     snd = wrap <$> readIndex 1 x
     tpl = lift2 Tuple fst snd
     result = PixelAspectRatio <$> tpl
    result

instance writeForeignPixelAspectRatio :: WriteForeign PixelAspectRatio where
  writeImpl (PixelAspectRatio (Tuple width height)) = writeImpl [unwrap width, unwrap height]
