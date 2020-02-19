module Rtsv2.Handler.Ingest
       (
         ingestInstances
       , ingestInstance
       , ingestStart
       , ingestStop
       , ingestInstancesMetrics
       ) where

import Prelude

import Data.Either (hush)
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..), fromMaybe')
import Data.Newtype (unwrap, wrap)
import Data.Tuple (Tuple(..))
import Erl.Data.List (List, nil, (:))
import Erl.Data.Tuple (tuple2)
import Erl.Process.Raw as Raw
import Erl.Utils as Timer
import Prometheus as Prometheus
import Rtsv2.Agents.IngestInstance as IngestInstance
import Rtsv2.Agents.IngestInstanceSup as IngestInstanceSup
import Rtsv2.Agents.IngestStats as IngestStats
import Rtsv2.Config as Config
import Rtsv2.Handler.MimeType as MimeType
import Rtsv2.Router.Endpoint (Canary)
import Rtsv2.Web.Bindings (streamAndVariant)
import Rtsv2.Web.Bindings as Bindings
import Shared.LlnwApiTypes (StreamIngestProtocol(..), StreamPublish, StreamDetails)
import Shared.Stream (IngestKey(..), ShortName, StreamAndVariant(..), StreamId, StreamRole(..), StreamVariant)
import Shared.Types.Agent.State as PublicState
import Shared.Types.Workflow.Metrics.Commmon (Stream)
import Shared.Utils (lazyCrashIfMissing)
import Simple.JSON (writeJSON)
import SpudGun (bodyToJSON)
import SpudGun as SpudGun
import Stetson (StetsonHandler)
import Stetson.Rest as Rest
import StetsonHelper (GenericStetsonGet, GenericStetsonGet2, genericGet, genericGet2, genericGetBy2)

ingestInstances :: StetsonHandler Unit
ingestInstances =
  Rest.handler (\req -> Rest.initResult req unit)
  # Rest.yeeha

ingestInstancesMetrics :: GenericStetsonGet2
ingestInstancesMetrics =
  genericGet2 nil ((MimeType.openmetrics getText) : (MimeType.json getJson) : nil)
  where
    getJson _ = do
      stats <- IngestStats.getStats
      pure $ writeJSON stats
    getText _ = do
      stats <- IngestStats.getStats
      pure $ statsToPrometheus stats

metrics :: List Prometheus.PrometheusMetric
metrics = { name: "ingest_frame_count"
          , help: "The number of frames processed in this stream"
          , metricType: Prometheus.Counter
          } :
          { name: "ingest_frames_per_second"
          , help: "The number of frames per second measured in this stream"
          , metricType: Prometheus.Gauge
          } :
          { name: "ingest_bitrate"
          , help: "The measured bitrate for this stream"
          , metricType: Prometheus.Gauge
          } :
          { name: "ingest_average_frame_size"
          , help: "The average size of frames in this stream"
          , metricType: Prometheus.Gauge
          } :
          { name: "ingest_byte_count"
          , help: "The number of bytes processed in this stream"
          , metricType: Prometheus.Counter
          } :
          { name: "ingest_last_dts"
          , help: "The last DTS value measured in this stream"
          , metricType: Prometheus.Counter
          } :
          { name: "ingest_last_pts"
          , help: "The last PTS value measured in this stream"
          , metricType: Prometheus.Counter
          } :
          { name: "ingest_last_capture_ms"
          , help: "The last Capture-MS value measured in this stream"
          , metricType: Prometheus.Counter
          } :
          { name: "ingest_bytes_sent"
          , help: "The total number of bytes to the client"
          , metricType: Prometheus.Counter
          } :
          { name: "ingest_bytes_received"
          , help: "The total number of bytes received from the client"
          , metricType: Prometheus.Counter
          } :
          { name: "ingest_last_bytes_read_report"
          , help: "The number of bytes in the last bytes-read report sent to the client"
          , metricType: Prometheus.Counter
          } :
          nil

-- TODO - rtmpStats
-- TODO - include streamAndVariant in labels
statsToPrometheus :: List (PublicState.IngestStats List) -> String
statsToPrometheus stats =
  foldl (\page { timestamp
               , ingestKey: IngestKey slotId streamRole profileId
               , streamBitrateMetrics
               , frameFlowMeterMetrics
               , rtmpIngestMetrics
               } ->
         let
           prometheusTimestamp = Prometheus.toTimestamp timestamp
         in
          page
          # streamBitrateMetricsToPrometheus prometheusTimestamp slotId profileId streamRole streamBitrateMetrics
          # frameFlowMetricsToPrometheus prometheusTimestamp slotId profileId streamRole frameFlowMeterMetrics
          # rtmpMetricsToPrometheus prometheusTimestamp slotId profileId streamRole rtmpIngestMetrics
        )
  (Prometheus.newPage metrics)
  stats
  # Prometheus.pageToString
  where
    streamBitrateMetricsToPrometheus timestamp slotId profileId streamRole streamBitrateMetrics page =
          foldl
          (\innerPage
            perStream@{ metrics: { frameCount
                                 , packetsPerSecond
                                 , bitrate
                                 , averagePacketSize }} ->
           let
             labels = labelsForStream slotId profileId streamRole perStream
           in
            innerPage
            # Prometheus.addMetric "ingest_frame_count" frameCount labels timestamp
            # Prometheus.addMetric "ingest_frames_per_second" packetsPerSecond labels timestamp
            # Prometheus.addMetric "ingest_bitrate" bitrate labels timestamp
            # Prometheus.addMetric "ingest_average_frame_size" averagePacketSize labels timestamp
          )
          page
          streamBitrateMetrics.perStreamMetrics

    frameFlowMetricsToPrometheus timestamp slotId profileId streamRole frameFlowMetrics page =
          foldl
          (\innerPage
            perStream@{ metrics: { byteCount
                                 , lastDts
                                 , lastPts
                                 , lastCaptureMs
                                 }} ->
           let
             labels = labelsForStream slotId profileId streamRole perStream
           in
            innerPage
            # Prometheus.addMetric "ingest_byte_count" byteCount labels timestamp
            # Prometheus.addMetric "ingest_last_dts" lastDts labels timestamp
            # Prometheus.addMetric "ingest_last_pts" lastPts labels timestamp
            # Prometheus.addMetric "ingest_last_capture_ms" lastCaptureMs labels timestamp
          )
          page
          frameFlowMetrics.perStreamMetrics

    rtmpMetricsToPrometheus timestamp slotId profileId streamRole {totalBytesSent, totalBytesReceived, lastBytesReadReport} page =
      let
        labels = Prometheus.toLabels $ (Tuple "slot" (Prometheus.toLabelValue (unwrap slotId))) :
                                       (Tuple "profile" (Prometheus.toLabelValue (unwrap profileId))) :
                                       (Tuple "role" (Prometheus.toLabelValue (show streamRole))) :
                                       nil
      in
       Prometheus.addMetric "ingest_bytes_sent" totalBytesSent labels timestamp page
       # Prometheus.addMetric "ingest_bytes_received" totalBytesReceived labels timestamp
       # Prometheus.addMetric "ingest_last_bytes_read_report" lastBytesReadReport labels timestamp

    labelsForStream :: forall a. StreamId -> StreamVariant -> StreamRole -> Stream a -> Prometheus.IOLabels
    labelsForStream slotId profileId role { streamId, frameType, profileName} =
      Prometheus.toLabels $ (Tuple "slot" (Prometheus.toLabelValue (unwrap slotId))) :
                            (Tuple "profile" (Prometheus.toLabelValue (unwrap profileId))) :
                            (Tuple "role" (Prometheus.toLabelValue (show role))) :
                            (Tuple "stream_id" (Prometheus.toLabelValue streamId)) :
                            (Tuple "frame_type" (Prometheus.toLabelValue (show frameType))) :
                            (Tuple "profile_name" (Prometheus.toLabelValue profileName)) :
                            nil

ingestInstance :: StreamId -> StreamVariant -> GenericStetsonGet (PublicState.Ingest List)
ingestInstance streamId variant = genericGet $ IngestInstance.getPublicState $ IngestKey streamId Primary variant

type IngestStartState = { shortName :: ShortName
                        , streamDetails :: Maybe StreamDetails
                        , streamPublish :: Maybe StreamPublish
                        , streamAndVariant :: StreamAndVariant
                        }

ingestStart :: Canary -> ShortName -> StreamAndVariant -> StetsonHandler IngestStartState
ingestStart canary shortName streamAndVariant =
  Rest.handler (\req ->
                 Rest.initResult req { shortName
                                     , streamAndVariant
                                     , streamDetails: Nothing
                                     , streamPublish: Nothing
                                     }
               )
  # Rest.serviceAvailable (\req state -> do
                            isAgentAvailable <- IngestInstanceSup.isAvailable
                            Rest.result isAgentAvailable req state)
  # Rest.resourceExists (\req state@{shortName, streamAndVariant: (StreamAndVariant _ variant)} ->
                          let
                            streamPublishPayload :: StreamPublish
                            streamPublishPayload = { host: "172.16.171.5"
                                                   , protocol: Rtmp
                                                   , shortname: unwrap shortName
                                                   , streamName: unwrap variant
                                                   , username: "user"}
                          in
                           do
                             {streamPublishUrl} <- Config.llnwApiConfig
                             restResult <- bodyToJSON <$> SpudGun.postJson (wrap streamPublishUrl) streamPublishPayload
                             let
                               streamDetails = hush $ restResult
                             Rest.result true req state{ streamDetails = streamDetails
                                                       , streamPublish = Just streamPublishPayload}
                          )
  -- TODO - hideous spawn here, but ingestInstance needs to do a monitor... - ideally we sleep forever and kill it in ingestStop...
  # Rest.contentTypesProvided (\req state ->
                                  Rest.result (tuple2 "text/plain" (\req2 state2@{ streamDetails: maybeStreamDetails
                                                                                 , streamAndVariant: StreamAndVariant streamId variantId
                                                                                 , streamPublish: maybeStreamPublish
                                                                                 } -> do
                                                                       pid <- Raw.spawn ((\_ -> Timer.sleep (wrap 10000))
                                                                                         { receive: Raw.receive
                                                                                         , receiveWithTimeout: Raw.receiveWithTimeout
                                                                                         })
                                                                       let
                                                                         streamDetails = fromMaybe' (lazyCrashIfMissing "stream_details missing") maybeStreamDetails
                                                                         streamPublish = fromMaybe' (lazyCrashIfMissing "stream_publish missing") maybeStreamPublish
                                                                         ingestKey = IngestKey streamId streamDetails.role variantId
                                                                       IngestInstanceSup.startIngest ingestKey streamPublish streamDetails "127.0.0.1" 0 pid
                                                                       Rest.result "ingestStarted" req2 state2
                                                                   ) : nil) req state)
  # Rest.yeeha


type IngestStopState = { ingestKey :: IngestKey }

ingestStop :: Canary -> StreamId -> StreamRole -> StreamVariant ->  StetsonHandler IngestStopState
ingestStop canary streamId role variant =
  Rest.handler (\req -> Rest.initResult req {ingestKey: IngestKey streamId role variant})

  # Rest.serviceAvailable (\req state -> do
                            isAgentAvailable <- IngestInstanceSup.isAvailable
                            Rest.result isAgentAvailable req state)

  # Rest.resourceExists (\req state@{ingestKey} -> do
                            isActive <- IngestInstance.isActive ingestKey
                            Rest.result isActive req state
                        )
  # Rest.contentTypesProvided (\req state ->
                                Rest.result (tuple2 "text/plain" (\req2 state2 -> do
                                                                     IngestInstance.stopIngest state.ingestKey
                                                                     Rest.result "ingestStopped" req2 state2
                                                                 ) : nil) req state)
  # Rest.yeeha
