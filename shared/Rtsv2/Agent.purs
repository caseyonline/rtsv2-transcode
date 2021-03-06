module Shared.Rtsv2.Agent
       (
         Agent(..)
       , SlotCharacteristics(..)
       , strToAgent
       , agentToStr
       , emptySlotCharacteristics
       ) where

import Prelude

import Control.Monad.Except (except)
import Data.Either (note)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.List.NonEmpty (singleton)
import Data.Maybe (Maybe(..), fromMaybe')
import Foreign (ForeignError(..), readString, unsafeToForeign)
import Shared.Utils (lazyCrashIfMissing)
import Simple.JSON (class ReadForeign, class WriteForeign)

--import Test.QuickCheck.Arbitrary (class Arbitrary, genericArbitrary)

data Agent = Egest
           | Ingest
           | IngestAggregator
           | StreamRelay
           | TestNode
           | TranscodeNode
           | IntraPoP
           | TransPoP

type SlotCharacteristics = { numProfiles :: Int
                           , totalBitrate :: Int
                           }

emptySlotCharacteristics :: SlotCharacteristics
emptySlotCharacteristics = { numProfiles: 0
                           , totalBitrate: 0
                           }

--------------------------------------------------------------------------------
-- Type class derivations
--------------------------------------------------------------------------------
derive instance genericAgent :: Generic Agent _
instance eqAgent :: Eq Agent where eq = genericEq
instance showAgent :: Show Agent where show = genericShow
instance readForeignAgent :: ReadForeign Agent where
  readImpl =
    readString >=> parseAgent
    where
      error s = singleton (ForeignError (errorString s))
      parseAgent s = except $ note (error s) (strToMaybeAgent s)
instance writForeignAgent :: WriteForeign Agent where
  writeImpl = agentToStr >>> unsafeToForeign

instance ordAgent :: Ord Agent where
  compare x y = compare (toInt x) (toInt y)
    where
      toInt TransPoP = 1
      toInt IntraPoP = 2
      toInt Ingest = 3
      toInt IngestAggregator = 4
      toInt StreamRelay = 5
      toInt Egest = 6
      toInt TestNode = 100
      toInt TranscodeNode = 101

strToAgent :: String -> Agent
strToAgent s =
  fromMaybe' (lazyCrashIfMissing $ errorString s) (strToMaybeAgent s)

strToMaybeAgent :: String -> Maybe Agent
strToMaybeAgent "Egest" = pure Egest
strToMaybeAgent "Ingest" = pure Ingest
strToMaybeAgent "IngestAggregator" = pure IngestAggregator
strToMaybeAgent "StreamRelay" = pure StreamRelay
strToMaybeAgent "TestNode" = pure TestNode
strToMaybeAgent "TranscodeNode" = pure TranscodeNode
strToMaybeAgent "IntraPoP" = pure IntraPoP
strToMaybeAgent "TransPoP" = pure TransPoP
strToMaybeAgent unknown = Nothing

agentToStr :: Agent -> String
agentToStr = show

errorString :: String -> String
errorString s = "Unknown Agent: " <> s
