module Shared.Types.Workflow.Metrics.RtmpPushIngest
       (
         Metrics
       ) where

type Metrics = { remoteIp :: String
               , remotePort :: Int
               , totalBytesSent :: Int
               , totalBytesReceived :: Int
               , lastBytesReadReport :: Int
               }
