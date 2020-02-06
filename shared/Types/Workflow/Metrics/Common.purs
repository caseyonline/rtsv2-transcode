module Shared.Types.Workflow.Metrics.Commmon
       (
         FrameType
       , Stream
       ) where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Newtype (class Newtype)
import Simple.JSON (class ReadForeign, class WriteForeign, writeImpl)
import Simple.JSON.Generics (enumSumRep)

data FrameType = Video
               | Audio
               | Subtitles
               | ProgramDetails
               | PCR

newtype Stream a = Stream
                   { streamId :: Int
                   , frameType :: FrameType
                   , profileName :: String
                   , metrics :: a
                   }

------------------------------------------------------------------------------
-- Type class derivations
------------------------------------------------------------------------------
derive instance genericFrameType :: Generic FrameType _
derive instance eqFrameType :: Eq FrameType
instance showFrameType :: Show FrameType where show = genericShow
instance readFrameType :: ReadForeign FrameType where readImpl = enumSumRep
instance writeForeignFrameType :: WriteForeign FrameType where
  writeImpl Video = writeImpl "video"
  writeImpl Audio = writeImpl "audio"
  writeImpl Subtitles = writeImpl "subtitles"
  writeImpl ProgramDetails = writeImpl "programDetails"
  writeImpl PCR = writeImpl "pcr"

derive instance newtypeStream :: Newtype (Stream a) _
derive newtype instance eqStream :: (Eq a) => Eq (Stream a)
derive newtype instance showStream :: (Show a) => Show (Stream a)
derive newtype instance readForeignStream :: (ReadForeign a) => ReadForeign (Stream a)
derive newtype instance writeForeignStream :: (WriteForeign a) => WriteForeign (Stream a)
