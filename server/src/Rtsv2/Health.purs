module Rtsv2.Health
       (
         Health(..)
       , percentageToHealth
       ) where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Foreign (unsafeToForeign)
import Rtsv2.Config (HealthConfig)
import Simple.JSON (class WriteForeign)

data Health = Perfect
            | Excellent
            | Good
            | Poor
            | Critical
            | NA

derive instance genericHealth :: Generic Health _

instance eqAgent :: Eq Health where
  eq = genericEq

instance showAgent :: Show Health where
  show = genericShow

instance foreignHealth :: WriteForeign Health where
  writeImpl = unsafeToForeign <<< show


percentageToHealth :: HealthConfig -> Int -> Health
percentageToHealth {thresholds} percentage
  | percentage >= thresholds.perfect = Perfect
  | percentage >  thresholds.excellent = Excellent
  | percentage >  thresholds.good = Good
  | percentage >  thresholds.poor = Poor
  | otherwise = Critical
