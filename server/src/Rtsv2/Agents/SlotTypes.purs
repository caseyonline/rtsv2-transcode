module Rtsv2.Agents.SlotTypes
       ( SlotConfiguration
       , SlotProfile
       , llnwStreamDetailsToSlotConfiguration
       )
       where

import Data.Int.Bits (shl, (.|.))
import Erl.Data.List (List, fromFoldable, mapWithIndex)
import Prelude ((+))
import Shared.Rtsv2.LlnwApiTypes as LlnwApiTypes
import Shared.Rtsv2.Stream (IngestKey(..), ProfileName, RtmpShortName, RtmpStreamName, SlotId, SlotRole)

type SlotConfiguration =
  { slotId :: SlotId
  , slotRole :: SlotRole
  , rtmpShortName :: RtmpShortName
  , profiles :: List SlotProfile
  , subscribeValidation :: Boolean
  }

type SlotProfile =
  { profileName :: ProfileName
  , streamName :: RtmpStreamName
  , ingestKey :: IngestKey
  , firstAudioSSRC :: Int
  , firstVideoSSRC :: Int
  , bitrate :: Int
  }

llnwStreamDetailsToSlotConfiguration :: RtmpShortName -> LlnwApiTypes.StreamDetails -> SlotConfiguration
llnwStreamDetailsToSlotConfiguration rtmpShortName {role, slot: {id, profiles, subscribeValidation}} =
  { slotId : id
  , slotRole: role
  , rtmpShortName
  , profiles: mapWithIndex (llwnSlotProfileToSlotProfile id role) (fromFoldable profiles)
  , subscribeValidation
  }

llwnSlotProfileToSlotProfile :: SlotId -> SlotRole -> LlnwApiTypes.SlotProfile -> Int -> SlotProfile
llwnSlotProfileToSlotProfile slotId slotRole (LlnwApiTypes.SlotProfile {name, rtmpStreamName, bitrate}) index =
  { profileName: name
  , ingestKey: IngestKey slotId slotRole name
  , streamName: rtmpStreamName
  , bitrate
  , firstAudioSSRC: audioSSRC (index + indexStandardOffset)
  , firstVideoSSRC: videoSSRC (index + indexStandardOffset)
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
