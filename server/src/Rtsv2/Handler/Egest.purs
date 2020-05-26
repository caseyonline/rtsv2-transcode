module Rtsv2.Handler.Egest
       ( startResource
       , egestInstancesMetrics
       ) where

import Prelude

import Data.Foldable (foldl)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Erl.Data.List (List, nil, (:))
import Prometheus as Prometheus
import Rtsv2.Agents.EgestInstance (CreateEgestPayload)
import Rtsv2.Agents.EgestInstanceSup as EgestInstanceSup
import Rtsv2.Agents.EgestStats as EgestStats
import Rtsv2.Config (LoadConfig)
import Rtsv2.Handler.MimeType as MimeType
import Shared.Rtsv2.JsonLd (EgestStats, EgestSessionStats)
import Shared.Rtsv2.JsonLd as JsonLd
import Shared.Rtsv2.Stream (EgestKey(..), SlotId, SlotRole)
import Shared.Rtsv2.Types (Server)
import Simple.JSON (writeJSON)
import StetsonHelper (GetHandler, PostHandler, multiMimeResponse, processPostPayload)

startResource :: LoadConfig -> PostHandler CreateEgestPayload
startResource loadConfig = processPostPayload (EgestInstanceSup.startLocalEgest loadConfig)

-- TODO: Vince - needed to return an Effect when calling statsToJson
egestInstancesMetrics :: Server -> GetHandler (List (EgestStats List))
egestInstancesMetrics thisServer = do
  multiMimeResponse ((MimeType.openmetrics statsToPrometheus) : (MimeType.json $ statsToJson thisServer) : nil) (Just <$> EgestStats.getStats)

statsToJson :: Server -> List (EgestStats List) -> String
statsToJson server statsList = do
  let stats = traverse
            (\stats@{egestKey : EgestKey slotId slotRole} -> do
              JsonLd.egestStatsNode slotId slotRole server stats
            ) statsList
  "sb"
  -- pure $ writeJSON stats

statsToPrometheus :: List (EgestStats List) -> String
statsToPrometheus stats =
  foldl (\page { timestamp
               , egestKey: EgestKey slotId slotRole
               , clientCount
               , sessions
               } ->
         let
           labels = labelsForSlot slotId slotRole
           prometheusTimestamp = Prometheus.toTimestamp timestamp
         in
          page
          # Prometheus.addMetric "client_count" clientCount labels prometheusTimestamp
          # sessionMetricsToPrometheus prometheusTimestamp slotId slotRole sessions
        )
  (Prometheus.newPage metrics)
  stats
  # Prometheus.pageToString
  where
    sessionMetricsToPrometheus prometheusTimestamp slotId slotRole sessions page =
      foldl
      (\innerPage session@{ audioPacketsSent
                          , audioOctetsSent
                          , videoPacketsSent
                          , videoOctetsSent
                          } ->
       let
         labels = labelsForSession slotId slotRole session
       in
        innerPage
        # Prometheus.addMetric "audio_packets_sent" audioPacketsSent labels prometheusTimestamp
        # Prometheus.addMetric "audio_octets_sent" audioOctetsSent labels prometheusTimestamp
        # Prometheus.addMetric "video_packets_sent" videoPacketsSent labels prometheusTimestamp
        # Prometheus.addMetric "video_octets_sent" videoOctetsSent labels prometheusTimestamp
      )
      page
      sessions

    labelsForSlot :: SlotId -> SlotRole -> Prometheus.IOLabels
    labelsForSlot slotId role =
      Prometheus.toLabels $ (Tuple "slot" (Prometheus.toLabelValue (show (unwrap slotId)))) :
                            (Tuple "role" (Prometheus.toLabelValue (show role))) :
                            nil

    labelsForSession :: SlotId -> SlotRole -> EgestSessionStats -> Prometheus.IOLabels
    labelsForSession slotId role {sessionId} =
      Prometheus.toLabels $ (Tuple "slot" (Prometheus.toLabelValue (show (unwrap slotId)))) :
                            (Tuple "role" (Prometheus.toLabelValue (show role))) :
                            (Tuple "sessionId" (Prometheus.toLabelValue sessionId)) :
                            nil


metrics :: List Prometheus.PrometheusMetric
metrics = { name: "client_count"
          , help: "The number of clients for this egest node"
          , metricType: Prometheus.Counter
          } :
          { name: "audio_packets_sent"
          , help: "The number of audio packets sent for this session"
          , metricType: Prometheus.Counter
          } :
          { name: "audio_octets_sent"
          , help: "The number of audio octets sent for this session"
          , metricType: Prometheus.Counter
          } :
          { name: "video_packets_sent"
          , help: "The number of video packets sent for this session"
          , metricType: Prometheus.Counter
          } :
          { name: "video_octets_sent"
          , help: "The number of video octets sent for this session"
          , metricType: Prometheus.Counter
          } :
          nil
