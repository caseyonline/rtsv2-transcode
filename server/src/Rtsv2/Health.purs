module Rtsv2.Health
       ( percentageToHealth
       ) where

import Prelude

import Rtsv2.Config (HealthConfig)
import Shared.Rtsv2.Types (Health(..))

percentageToHealth :: HealthConfig -> Int -> Health
percentageToHealth {thresholds} percentage
  | percentage >= thresholds.perfect = Perfect
  | percentage >  thresholds.excellent = Excellent
  | percentage >  thresholds.good = Good
  | percentage >  thresholds.poor = Poor
  | otherwise = Critical
