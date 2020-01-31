module Rtsv2App.Data.PreciseDateTime where

import Prelude

import Data.DateTime (DateTime)
import Data.Either (Either, note)
import Data.Formatter.DateTime (FormatterCommand(..), format)
import Data.List (List, fromFoldable)
import Data.Newtype (class Newtype, unwrap)
import Data.PreciseDateTime as PDT
import Data.RFC3339String (RFC3339String(..))

newtype PreciseDateTime = PreciseDateTime PDT.PreciseDateTime

derive instance newtypePreciseDateTime :: Newtype PreciseDateTime _

-- | Try to parse a `PreciseDateTime` from a string.
fromString :: String -> Either String PreciseDateTime
fromString = 
  map PreciseDateTime 
    <<< note "Could not parse RFC339 string" 
    <<< PDT.fromRFC3339String 
    <<< RFC3339String

-- | Convert a precise datetime into a less-precise JS-based datetime
toDateTime :: PreciseDateTime -> DateTime
toDateTime = unwrap >>> PDT.toDateTimeLossy

-- | Convert a precise datetime into a string representation according to RFC3339
toRFC3339String :: PreciseDateTime -> RFC3339String
toRFC3339String = unwrap >>> PDT.toRFC3339String

-- | Display a human-readable version of the precise datetime, as described in the Rtsv2App spec
-- |
-- | Example: "Wed Nov 5, 1999"
toDisplayWeekName :: PreciseDateTime -> String
toDisplayWeekName = toDateTime >>> format dateFormatter
  where
  dateFormatter :: List FormatterCommand
  dateFormatter = fromFoldable
    [ DayOfWeekNameShort
    , Placeholder " "
    , MonthShort
    , Placeholder " "
    , DayOfMonth
    , Placeholder ", "
    , YearFull
    ]

-- | An alternate way to display a human-readable version of the precise datetime
-- |
-- | Example: "November 5, 1999"
toDisplayMonthDayYear :: PreciseDateTime -> String
toDisplayMonthDayYear = toDateTime >>> format dateFormatter
  where
  dateFormatter :: List FormatterCommand
  dateFormatter = fromFoldable
    [ MonthFull
    , Placeholder " "
    , DayOfMonth
    , Placeholder ", "
    , YearFull
    ]
