module Shared.Types.Media.Types.Avc
       (
         AvcProfile(..)
       , AvcLevel
       ) where

import Prelude

import Control.Monad.Except (except)
import Data.Either (note)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Ord (genericCompare)
import Data.Generic.Rep.Show (genericShow)
import Data.List.NonEmpty (singleton)
import Data.Maybe (Maybe(..))
import Foreign (ForeignError(..), readString, unsafeToForeign)
import Simple.JSON (class ReadForeign, class WriteForeign)

data AvcProfile = High
                | Main
                | Baseline

newtype AvcLevel = AvcLevel Number

--------------------------------------------------------------------------------
-- Type class derivations
--------------------------------------------------------------------------------
-- AvcProfile
derive instance genericAvcProfile :: Generic AvcProfile _
instance eqAvcProfile :: Eq AvcProfile where eq = genericEq
instance compareAvcProfile :: Ord AvcProfile where compare = genericCompare
instance showAvcProfile :: Show AvcProfile where show = genericShow
instance readForeignAvcProfile :: ReadForeign AvcProfile where
  readImpl =
    readString >=> parseAvcProfile
    where
      error s = singleton (ForeignError (errorString s))
      parseAvcProfile s = except $ note (error s) (toType s)
      toType "high" = pure High
      toType "main" = pure Main
      toType "baseline" = pure Baseline
      toType unknown = Nothing
      errorString s = "Unknown AvcProfile: " <> s
instance writeForeignAvcProfile :: WriteForeign AvcProfile where
  writeImpl =
    toString >>> unsafeToForeign
    where
      toString High = "high"
      toString Main = "main"
      toString Baseline = "baseline"

-- AvcLevel
derive newtype instance readForeignAvcLevel :: ReadForeign AvcLevel
derive newtype instance writeForeignAvcLevel :: WriteForeign AvcLevel
