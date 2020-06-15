module Rtsv2.Handler.Ingest
       ( ingestInstance
       , ingestStart
       , ingestStop
       , ingestInstancesMetrics
       ) where

import Prelude

import Data.Either (Either(..), hush)
import Data.Foldable (foldl, find)
import Data.Maybe (Maybe(..), fromMaybe', isJust)
import Data.Newtype (unwrap, wrap)
import Data.Traversable (traverse_)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Erl.Atom (Atom, atom)
import Erl.Cowboy.Req (StatusCode(..), IpAddress)
import Erl.Cowboy.Req as Req
import Erl.Data.List (List, nil, (:))
import Erl.Data.List as List
import Erl.Data.Map as Map
import Erl.Data.Tuple (fst, tuple2, uncurry4)
import Erl.Process.Raw (Pid)
import Erl.Process.Raw as Raw
import Erl.Utils (self)
import Gproc as GProc
import Gproc as Gproc
import Logger as Logger
import Prometheus as Prometheus
import Rtsv2.Agents.IngestInstance as IngestInstance
import Rtsv2.Agents.IngestInstanceSup as IngestInstanceSup
import Rtsv2.Agents.IngestStats as IngestStats
import Rtsv2.Agents.IngestSup as IngestSup
import Rtsv2.Config (LoadConfig)
import Rtsv2.Config as Config
import Rtsv2.Handler.MimeType as MimeType
import Rtsv2.LlnwApi as LlnwApi
import Rtsv2.Types (ResourceFailed(..))
import Rtsv2.Utils (badargToMaybe)
import Shared.Rtsv2.Agent.State (IngestStats)
import Shared.Rtsv2.Agent.State as PublicState
import Shared.Rtsv2.LlnwApiTypes (StreamIngestProtocol(..), StreamPublish, StreamDetails, SlotProfile(..))
import Shared.Rtsv2.Stream (IngestKey(..), ProfileName, RtmpShortName, RtmpStreamName, SlotId, SlotRole)
import Shared.Rtsv2.Types (CanaryState)
import Shared.Types.Workflow.Metrics.Commmon (Stream)
import Shared.Utils (lazyCrashIfMissing)
import Simple.JSON (writeJSON)
import Stetson (StetsonHandler)
import Stetson.Rest as Rest
import StetsonHelper (GetHandler, jsonResponse, multiMimeResponse)

ingestInstancesMetrics :: GetHandler (List (IngestStats List))
ingestInstancesMetrics =
  multiMimeResponse ((MimeType.openmetrics (pure <<< statsToPrometheus)) : (MimeType.json (pure <<< writeJSON)) : nil) (Just <$> IngestStats.getStats)

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

statsToPrometheus :: List (PublicState.IngestStats List) -> String
statsToPrometheus stats =
  foldl (\page { timestamp
               , ingestKey: IngestKey slotId slotRole profileId
               , streamBitrateMetrics
               , frameFlowMeterMetrics
               , rtmpIngestMetrics
               } ->
         let
           prometheusTimestamp = Prometheus.toTimestamp timestamp
         in
          page
          # streamBitrateMetricsToPrometheus prometheusTimestamp slotId profileId slotRole streamBitrateMetrics
          # frameFlowMetricsToPrometheus prometheusTimestamp slotId profileId slotRole frameFlowMeterMetrics
          # rtmpMetricsToPrometheus prometheusTimestamp slotId profileId slotRole rtmpIngestMetrics
        )
  (Prometheus.newPage metrics)
  stats
  # Prometheus.pageToString
  where
    streamBitrateMetricsToPrometheus timestamp slotId profileId slotRole streamBitrateMetrics page =
          foldl
          (\innerPage
            perStream@{ metrics: { frameCount
                                 , packetsPerSecond
                                 , bitrate
                                 , averagePacketSize }} ->
           let
             labels = labelsForStream slotId profileId slotRole perStream
           in
            innerPage
            # Prometheus.addMetric "ingest_frame_count" frameCount labels timestamp
            # Prometheus.addMetric "ingest_frames_per_second" packetsPerSecond labels timestamp
            # Prometheus.addMetric "ingest_bitrate" bitrate labels timestamp
            # Prometheus.addMetric "ingest_average_frame_size" averagePacketSize labels timestamp
          )
          page
          streamBitrateMetrics.perStreamMetrics

    frameFlowMetricsToPrometheus timestamp slotId profileId slotRole frameFlowMetrics page =
          foldl
          (\innerPage
            perStream@{ metrics: { byteCount
                                 , lastDts
                                 , lastPts
                                 , lastCaptureMs
                                 }} ->
           let
             labels = labelsForStream slotId profileId slotRole perStream
           in
            innerPage
            # Prometheus.addMetric "ingest_byte_count" byteCount labels timestamp
            # Prometheus.addMetric "ingest_last_dts" lastDts labels timestamp
            # Prometheus.addMetric "ingest_last_pts" lastPts labels timestamp
            # Prometheus.addMetric "ingest_last_capture_ms" lastCaptureMs labels timestamp
          )
          page
          frameFlowMetrics.perStreamMetrics

    rtmpMetricsToPrometheus timestamp slotId profileId slotRole {totalBytesSent, totalBytesReceived, lastBytesReadReport} page =
      let
        labels = Prometheus.toLabels $ (Tuple "slot" (Prometheus.toLabelValue (show (unwrap slotId)))) :
                                       (Tuple "profile" (Prometheus.toLabelValue (unwrap profileId))) :
                                       (Tuple "role" (Prometheus.toLabelValue (show slotRole))) :
                                       nil
      in
       Prometheus.addMetric "ingest_bytes_sent" totalBytesSent labels timestamp page
       # Prometheus.addMetric "ingest_bytes_received" totalBytesReceived labels timestamp
       # Prometheus.addMetric "ingest_last_bytes_read_report" lastBytesReadReport labels timestamp

    labelsForStream :: forall a. SlotId -> ProfileName -> SlotRole -> Stream a -> Prometheus.IOLabels
    labelsForStream slotId profileId role { streamId, frameType, profileName} =
      Prometheus.toLabels $ (Tuple "slot" (Prometheus.toLabelValue (show (unwrap slotId)))) :
                            (Tuple "profile" (Prometheus.toLabelValue (unwrap profileId))) :
                            (Tuple "role" (Prometheus.toLabelValue (show role))) :
                            (Tuple "stream_id" (Prometheus.toLabelValue streamId)) :
                            (Tuple "frame_type" (Prometheus.toLabelValue (show frameType))) :
                            (Tuple "profile_name" (Prometheus.toLabelValue profileName)) :
                            nil

ingestInstance :: SlotId -> SlotRole -> ProfileName -> GetHandler (PublicState.Ingest List)
ingestInstance slotId slotRole profileName =
  jsonResponse $ Just <$> (IngestInstance.getPublicState (IngestKey slotId slotRole profileName))


type IngestStartState = { streamDetails :: Maybe StreamDetails
                        , streamPublish :: Maybe StreamPublish
                        }

ingestStart :: LoadConfig -> CanaryState -> RtmpShortName -> RtmpStreamName -> StetsonHandler IngestStartState
ingestStart loadConfig canary shortName streamName =
  Rest.handler (\req ->
                 Rest.initResult req { streamDetails: Nothing
                                     , streamPublish: Nothing
                                     }
               )
  # Rest.serviceAvailable (\req state -> do
                            isAgentAvailable <- IngestSup.isAgentAvailable
                            Rest.result isAgentAvailable req state)
  # Rest.resourceExists (\req state ->
                          let
                            streamPublishPayload :: StreamPublish
                            streamPublishPayload = wrap { host: "172.16.171.5"
                                                        , protocol: Rtmp
                                                        , rtmpShortName: shortName
                                                        , rtmpStreamName: streamName
                                                        , username: "user"
                                                        , clientIp: ipToString $ fst $ Req.peer req
                                                        }
                          in
                           do
                             config <- Config.llnwApiConfig
                             restResult <- LlnwApi.streamPublish config streamPublishPayload
                             let
                               streamDetails = hush $ restResult
                             Rest.result (isJust streamDetails) req state{ streamDetails = streamDetails
                                                                         , streamPublish = Just streamPublishPayload}
                          )
  # Rest.contentTypesProvided (\req state ->
                                  Rest.result (tuple2 "text/plain" (\req2 state2@{ streamDetails: maybeStreamDetails
                                                                                 , streamPublish: maybeStreamPublish
                                                                                 } -> do
                                                                       let
                                                                         streamDetails = fromMaybe' (lazyCrashIfMissing "stream_details missing") maybeStreamDetails
                                                                         streamPublish = fromMaybe' (lazyCrashIfMissing "stream_publish missing") maybeStreamPublish
                                                                         findProfile = fromMaybe' (lazyCrashIfMissing "profile_details missing") $ find (\ (SlotProfile { rtmpStreamName: profileStreamName }) -> profileStreamName == streamName) streamDetails.slot.profiles
                                                                         (SlotProfile { name: profileName }) = findProfile
                                                                         ingestKey = IngestKey streamDetails.slot.id streamDetails.role profileName
                                                                       pid <- startFakeIngest ingestKey
                                                                       maybeStarted <- IngestInstanceSup.startLocalRtmpIngest loadConfig canary ingestKey streamPublish streamDetails "127.0.0.1" 0 pid
                                                                       logInfo "IngestInstanceSup returned" {maybeStarted, canary}
                                                                       case maybeStarted of
                                                                         Right _ ->
                                                                           Rest.result "ingestStarted" req2 state2
                                                                         Left error -> do
                                                                           case error of
                                                                             AlreadyRunning ->
                                                                               pure unit
                                                                             _ ->
                                                                                -- No need to stop the fake ingest, since its gproc.register will have failed
                                                                                stopFakeIngest ingestKey
                                                                           req3 <- Req.reply (StatusCode 409) Map.empty ("ingestStartFailed" <> (show error)) req2
                                                                           Rest.stop req3 state2
                                                                   ) : nil) req state)
  # Rest.yeeha


type IngestStopState = { ingestKey :: IngestKey }

ingestStop :: SlotId -> SlotRole -> ProfileName -> StetsonHandler IngestStopState
ingestStop slotId role profileName =
  Rest.handler (\req -> Rest.initResult req {ingestKey: IngestKey slotId role profileName})

  # Rest.serviceAvailable (\req state -> do
                            isAgentAvailable <- IngestSup.isAgentAvailable
                            Rest.result isAgentAvailable req state)

  # Rest.resourceExists (\req state@{ingestKey} -> do
                            isActive <- IngestInstance.isActive ingestKey
                            stopFakeIngest state.ingestKey
                            Rest.result isActive req state
                        )
  # Rest.contentTypesProvided (\req state ->
                                Rest.result (tuple2 "text/plain" (\req2 state2 -> do
                                                                     stopFakeIngest state.ingestKey
                                                                     Rest.result "ingestStopped" req2 state2
                                                                 ) : nil) req state)
  # Rest.yeeha

startFakeIngest :: IngestKey -> Effect Pid
startFakeIngest ingestKey = do
  let
    proc parent = do
      self <- self
      logInfo "fake ingest running" {ingestKey, self}
      registerRes <- badargToMaybe $ GProc.register (tuple2 (atom "test_ingest_client") ingestKey)
      Raw.send parent (atom "running")
      case registerRes of
        Nothing -> pure unit
        Just _ -> do
          handlerLoop ingestKey
          logInfo "fake ingest stopped" {}
          pure unit
  parent <- self
  child <- Raw.spawn (proc parent)
  void $ Raw.receive
  pure child

handlerLoop :: IngestKey -> Effect Unit
handlerLoop ingestKey = do
  x <- Raw.receive
  if (x == (atom "stop"))
    then pure unit
    else handlerLoop ingestKey

stopFakeIngest :: IngestKey -> Effect Unit
stopFakeIngest ingestKey = do
  pid <- Gproc.whereIs (tuple2 (atom "test_ingest_client") ingestKey)
  logInfo "stopping fake ingest" {ingestKey, pid}
  traverse_ ((flip Raw.send) (atom "stop")) pid
  pure unit

ipToString :: IpAddress -> String
ipToString ip =
  uncurry4 doToString ip
  where
    doToString a b c d =
      (octetToString a) <> "." <> (octetToString b) <> "." <> (octetToString c) <> "." <> (octetToString d)
    octetToString octet =
      show octet

--------------------------------------------------------------------------------
-- Log helpers
--------------------------------------------------------------------------------
domain :: List Atom
domain = atom <$> ("Client" :  "Instance" : List.nil)

logInfo :: forall report. String -> { | report } -> Effect Unit
logInfo = Logger.info <<< Logger.traceMetadata domain
