module Shared.Agent (
                Agent(..)
              , strToAgent
              , agentToStr
              ) where

import Prelude

import Control.Monad.Except (except)
import Data.Either (note)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.List.NonEmpty (singleton)
import Data.Maybe (Maybe(..), fromMaybe')
import Foreign (ForeignError(..), readString)
import Shared.Utils (lazyCrashIfMissing)
import Simple.JSON (class ReadForeign)

--import Test.QuickCheck.Arbitrary (class Arbitrary, genericArbitrary)

data Agent = Egest
           | Ingest
           | IngestAggregator
           | StreamRelay
           | IntraPoP
           | TransPoP

--------------------------------------------------------------------------------
-- Type class derivations
--------------------------------------------------------------------------------
derive instance genericAgent :: Generic Agent _

instance eqAgent :: Eq Agent where
  eq = genericEq

instance showAgent :: Show Agent where
  show = genericShow

instance foreignAgent :: ReadForeign Agent where
  readImpl =
    readString >=> parseAgent
    where
      error s = singleton (ForeignError (errorString s))
      parseAgent s = except $ note (error s) (strToMaybeAgent s)

strToAgent :: String -> Agent
strToAgent s =
  fromMaybe' (lazyCrashIfMissing $ errorString s) (strToMaybeAgent s)

strToMaybeAgent :: String -> Maybe Agent
strToMaybeAgent "Egest" = pure Egest
strToMaybeAgent "Ingest" = pure Ingest
strToMaybeAgent "IngestAggregator" = pure IngestAggregator
strToMaybeAgent "StreamRelay" = pure StreamRelay
strToMaybeAgent "IntraPoP" = pure IntraPoP
strToMaybeAgent "TransPoP" = pure TransPoP
strToMaybeAgent unknown = Nothing

agentToStr :: Agent -> String
agentToStr = show

errorString :: String -> String
errorString s = "Unknown Agent: " <> s
