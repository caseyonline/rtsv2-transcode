module Rtsv2.Agents.SlotTypes
       ( SlotConfiguration
       , SlotProfile
       , llnwStreamDetailsToSlotConfiguration
       , MaybeVideo
       )
       where

import Prelude

import Data.Foldable (all)
import Data.Int.Bits (shl, (.|.))
import Data.Maybe (Maybe(..))
import Data.Undefinable (Undefinable, toMaybe, toUndefinable)
import Erl.Data.List (List, fromFoldable, mapWithIndex)
import Shared.Rtsv2.LlnwApiTypes as LlnwApiTypes
import Shared.Rtsv2.Stream (IngestKey(..), ProfileName, RtmpShortName, RtmpStreamName, SlotId, SlotRole)
import Simple.JSON (class ReadForeign, class WriteForeign, readImpl, writeImpl)

type SlotConfiguration =
  { slotId :: SlotId
  , slotRole :: SlotRole
  , rtmpShortName :: RtmpShortName
  , profiles :: List SlotProfile
  , subscribeValidation :: Boolean
  , audioOnly :: Boolean
  }

type SlotProfile =
  { profileName :: ProfileName
  , streamName :: RtmpStreamName
  , ingestKey :: IngestKey
  , firstAudioSSRC :: Int
  , firstVideoSSRC :: MaybeVideo Int -- this is really just Undefinable but that doesn't have a WriteForeign instance
  , bitrate :: Int
  }

newtype MaybeVideo a = MaybeVideo (Undefinable a)

-- derive instance genericDownstreamWsMessage :: Generic DownstreamWsMessage _

instance readForeignMaybeVideo :: ReadForeign a => ReadForeign (MaybeVideo a) where
  readImpl o = (MaybeVideo <<< toUndefinable) <$> readImpl o

instance writeForeignMaybeVideo :: WriteForeign a => WriteForeign (MaybeVideo a) where
  writeImpl (MaybeVideo msg) = writeImpl (toMaybe msg)


llnwStreamDetailsToSlotConfiguration :: RtmpShortName -> LlnwApiTypes.StreamDetails -> SlotConfiguration
llnwStreamDetailsToSlotConfiguration rtmpShortName {role, slot: {id, profiles, subscribeValidation}} =
  let
    audioOnly = all (\(LlnwApiTypes.SlotProfile {videoBitrate}) -> videoBitrate == 0) profiles
  in
    { slotId : id
    , slotRole: role
    , rtmpShortName
    , profiles: mapWithIndex (llwnSlotProfileToSlotProfile audioOnly id role) (fromFoldable profiles)
    , audioOnly
    , subscribeValidation
    }

llwnSlotProfileToSlotProfile :: Boolean -> SlotId -> SlotRole -> LlnwApiTypes.SlotProfile -> Int -> SlotProfile
llwnSlotProfileToSlotProfile audioOnly slotId slotRole (LlnwApiTypes.SlotProfile {name, rtmpStreamName, audioBitrate, videoBitrate}) index =
  { profileName: name
  , ingestKey: IngestKey slotId slotRole name
  , streamName: rtmpStreamName
  , bitrate: audioBitrate + videoBitrate
  , firstAudioSSRC: audioSSRC (index + indexStandardOffset)
  , firstVideoSSRC:
      MaybeVideo $ toUndefinable
        if videoBitrate == 0 then Nothing
        else Just $ videoSSRC (index + indexStandardOffset)
  }

-- Bit-shifting logic and constants mirrored in rtsv2_rtp.hrl

audioTag :: Int
audioTag = 1

videoTag :: Int
videoTag = 2

streamIndex :: Int
streamIndex = 0

indexStandardOffset :: Int
indexStandardOffset = 16

audioSSRC :: Int -> Int
audioSSRC index =
  (shl index 16) .|. (shl audioTag 8) .|. streamIndex

videoSSRC :: Int -> Int
videoSSRC index =
  (shl index 16) .|. (shl videoTag 8) .|. streamIndex
