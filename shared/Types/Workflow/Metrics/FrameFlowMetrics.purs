module Shared.Types.Workflow.Metrics.FrameFlow
       (
         StreamMetrics
       , Metrics
       ) where

import Data.Maybe (Maybe)
import Shared.Types.Workflow.Metrics.Commmon (Stream)

type StreamMetrics = { frameCount :: Int
                     , byteCount :: Int
                     , lastDts :: Int
                     , lastPts :: Int
                     , lastCaptureMs :: Maybe Int
                     , codec :: Maybe String
                     }

type Metrics f = { perStreamMetrics :: f (Stream StreamMetrics)
                 }
