module Rtsv2App.Data.Log
  ( LogReason(..)
  , message
  , reason
  , Log -- no constructors exported
  , mkLog
  ) where

import Prelude

import Rtsv2App.Capability.Now (class Now, nowDateTime)
import Data.DateTime (DateTime)
import Data.Either (either)
import Data.Formatter.DateTime (formatDateTime)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------
data LogReason = Debug | Info | Warn | Error

derive instance genericLogReason :: Generic LogReason _
derive instance eqLogReason :: Eq LogReason
derive instance ordLogReason :: Ord LogReason

instance showLogReason :: Show LogReason where
  show = genericShow

newtype Log = Log
  { reason :: LogReason 
  , timestamp :: DateTime
  , message :: String
  }

derive instance genericLog :: Generic Log _
derive instance eqLog :: Eq Log

-------------------------------------------------------------------------------
-- Functions
-------------------------------------------------------------------------------
message :: Log -> String
message (Log { message: m }) = m

-- | This helper function retrieves the reason a log was produced from a `Log`.
reason :: Log -> LogReason
reason (Log { reason: r }) = r

-- | This helper function retrieves the time a `Log` was produced.
timestamp :: Log -> DateTime
timestamp (Log { timestamp: t }) = t

mkLog :: ∀ m. Now m => LogReason -> String -> m Log
mkLog logReason inputMessage = do
  now <- nowDateTime

  let 
    -- Will produce a header like "{DEBUG: 2018-10-25 11:25:29 AM]\nMessage contents..."
    headerWith start = 
      "[" <> start <> ": " <> formatTimestamp now <> "]\n" <> inputMessage

    -- Writes the header with the correct log reason  
    formattedLog = case logReason of
      Debug -> headerWith "DEBUG"
      Info -> headerWith "INFO"
      Warn -> headerWith "WARNING"
      Error -> headerWith "ERROR"
    
  pure $ Log { reason: logReason, timestamp: now, message: formattedLog }

  where
  -- Will format "2018-10-25 11:25:29 AM"
  formatTimestamp = 
    either (const "(Failed to assign time)") identity 
    <<< formatDateTime "YYYY-DD-MM hh:mm:ss a"
