module Shared.Types.Workflow.Metrics.StreamBitrateMonitor
       (
         StreamMetrics
       , Metrics
       ) where

import Shared.Types.Workflow.Metrics.Commmon (Stream)

type StreamMetrics = { frameCount :: Int
                     , packetsPerSecond :: Number
                     , bitrate :: Int
                     , averagePacketSize :: Number
                     }

type Metrics f = { windowSize :: Int
                 , notificationFrequency :: Int
                 , perStreamMetrics :: f (Stream StreamMetrics)
                 }
