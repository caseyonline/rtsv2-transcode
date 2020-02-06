module Shared.Types.Workflow.Metrics.RtmpPushIngest
       (
         Metrics
       ) where

import Prelude

import Data.Newtype (class Newtype)
import Simple.JSON (class ReadForeign, class WriteForeign)

newtype Metrics = Metrics
                  { remoteIp :: String
                  , remotePort :: Int
                  , bytesRead :: Int
                  }

------------------------------------------------------------------------------
-- Type class derivations
------------------------------------------------------------------------------
derive instance newtypeMetrics :: Newtype Metrics _
derive newtype instance eqMetrics :: Eq Metrics
derive newtype instance showMetrics :: Show Metrics
derive newtype instance readForeignMetrics :: ReadForeign Metrics
derive newtype instance writeForeignMetrics :: WriteForeign Metrics
