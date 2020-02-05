module Shared.Types.Workflow.Metrics.StreamBitrateMonitor
       (
         StreamMetrics
       , Metrics
       ) where

import Prelude

import Data.Foldable (class Foldable)
import Data.Newtype (class Newtype)
import Data.Unfoldable (class Unfoldable)
import Shared.Types (Container)
import Shared.Types.Workflow.Metrics.Commmon (Stream)
import Simple.JSON (class ReadForeign, class WriteForeign)

newtype StreamMetrics = StreamMetrics
                        { frameCount :: Int
                        , packetsPerSecond :: Number
                        , bitrate :: Int
                        , averagePacketSize :: Number
                        }

newtype Metrics f = Metrics
                    { windowSize :: Int
                    , notificationFrequency :: Int
                    , perStreamMetrics :: Container f (Stream StreamMetrics)
                    }

------------------------------------------------------------------------------
-- Type class derivations
------------------------------------------------------------------------------
derive instance newtypeStreamMetrics :: Newtype StreamMetrics _
derive newtype instance eqStreamMetrics :: Eq StreamMetrics
derive newtype instance showStreamMetrics :: Show StreamMetrics
derive newtype instance readForeignStreamMetrics :: ReadForeign StreamMetrics
derive newtype instance writeForeignStreamMetrics :: WriteForeign StreamMetrics

derive instance newtypeMetrics :: Newtype (Metrics f) _
derive newtype instance eqMetrics :: (Eq (f (Stream StreamMetrics))) => Eq (Metrics f)
derive newtype instance showMetrics :: (Show (f (Stream StreamMetrics))) => Show (Metrics f)
derive newtype instance readForeignMetrics :: (Unfoldable f) => ReadForeign (Metrics f)
derive newtype instance writeForeignMetrics :: (Foldable f) => WriteForeign (Metrics f)
