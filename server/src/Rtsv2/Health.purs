module Rtsv2.Health
       ( percentageToHealth
       ) where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Foreign (unsafeToForeign)
import Rtsv2.Config (HealthConfig)
import Shared.Rtsv2.Types (Health(..))
import Simple.JSON (class WriteForeign)

percentageToHealth :: HealthConfig -> Int -> Health
percentageToHealth {thresholds} percentage
  | percentage >= thresholds.perfect = Perfect
  | percentage >  thresholds.excellent = Excellent
  | percentage >  thresholds.good = Good
  | percentage >  thresholds.poor = Poor
  | otherwise = Critical
