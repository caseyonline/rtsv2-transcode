module RTCPeerConnection
       ( getVideoStats
       , getAudioStats
       , VideoRTCPeerConnectionStats(..)
       , AudioRTCPeerConnectionStats(..)
       ) where

import Prelude

import Control.Promise (Promise)
import Control.Promise as Promise
import Effect (Effect)
import Effect.Aff (Aff)
import Foreign (Foreign, unsafeFromForeign)
import Toppokki (Page)

type VideoRTCPeerConnectionStats =
  { id :: String
  , timestamp :: Number
  , type :: String
  , ssrc :: Int
  , isRemote :: Boolean
  , mediaType :: String
  , kind :: String
  , trackId :: String
  , transportId :: String
  , codecId :: String
  , firCount :: Int
  , pliCount :: Int
  , nackCount :: Int
  , packetsReceived :: Int
  , bytesReceived :: Int
  , headerBytesReceived :: Int
  , packetsLost :: Int
  , lastPacketReceivedTimestamp :: Number
  , framesDecoded :: Int
  , keyFramesDecoded :: Int
  , totalDecodeTime :: Number
  , decoderImplementation :: String
  }

type AudioRTCPeerConnectionStats =
  { id :: String
  , timestamp :: Number
  , type :: String
  , trackIdentifier :: String
  , remoteSource :: Boolean
  , ended :: Boolean
  , detached :: Boolean
  , kind :: String
  , jitterBufferDelay :: Number
  , jitterBufferEmittedCount :: Int
  , audioLevel :: Int
  , totalAudioEnergy :: Int
  , totalSamplesReceived :: Int
  , totalSamplesDuration :: Number
  , concealedSamples :: Int
  , silentConcealedSamples :: Int
  , concealmentEvents :: Int
  , insertedSamplesForDeceleration :: Int
  , removedSamplesForAcceleration :: Int
  }

getVideoStats :: Page -> Aff VideoRTCPeerConnectionStats
getVideoStats p = do
  raw <- (Promise.toAffE $ _getVideoStats p)
  pure $ unsafeFromForeign raw

getAudioStats :: Page -> Aff AudioRTCPeerConnectionStats
getAudioStats p = do
  raw <- (Promise.toAffE $ _getAudioStats p)
  pure $ unsafeFromForeign raw

foreign import _getVideoStats :: Page -> (Effect (Promise Foreign))
foreign import _getAudioStats :: Page -> (Effect (Promise Foreign))
