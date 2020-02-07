module Shared.Types.Workflow.Metrics.RtmpPushIngest
       (
         Metrics
       ) where

type Metrics = { remoteIp :: String
               , remotePort :: Int
               , bytesRead :: Int
               }
