module Shared.Types.Workflow.Metrics.FrameFlow
       (
         StreamMetrics
       , Metrics
       ) where

import Shared.Types.Workflow.Metrics.Commmon (Stream)

type StreamMetrics = { frameCount :: Int
                     , byteCount :: Int
                     , lastDts :: Int
                     , lastPts :: Int
                     , lastCaptureMs :: Int
                     , codec :: String
                     }

type Metrics f = { perStreamMetrics :: f (Stream StreamMetrics)
                 }
