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

percentageToHealth :: Int -> Health
percentageToHealth percentage
  | percentage == 100 = Perfect
  | percentage > 75 = Excellent
  | percentage > 50 = Good
  | percentage > 25 = Poor
  | otherwise = Critical
