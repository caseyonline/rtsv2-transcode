module Shared.Types.Workflow.Metrics.Commmon
       (
         FrameType
       , Stream
       ) where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Simple.JSON (class ReadForeign, class WriteForeign, writeImpl)
import Simple.JSON.Generics (enumSumRep)

data FrameType = Video
               | Audio
               | Subtitles
               | ProgramDetails
               | PCR

type Stream a = { streamId :: Int
                , frameType :: FrameType
                , profileName :: String
                , metrics :: a
                }

------------------------------------------------------------------------------
-- Type class derivations
------------------------------------------------------------------------------
derive instance genericFrameType :: Generic FrameType _
derive instance eqFrameType :: Eq FrameType
instance showFrameType :: Show FrameType where
  show Video = "video"
  show Audio = "audio"
  show Subtitles = "subtitles"
  show ProgramDetails = "program_details"
  show PCR = "pcr"
instance readFrameType :: ReadForeign FrameType where readImpl = enumSumRep
instance writeForeignFrameType :: WriteForeign FrameType where
  writeImpl Video = writeImpl "video"
  writeImpl Audio = writeImpl "audio"
  writeImpl Subtitles = writeImpl "subtitles"
  writeImpl ProgramDetails = writeImpl "programDetails"
  writeImpl PCR = writeImpl "pcr"
