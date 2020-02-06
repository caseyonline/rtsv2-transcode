module Rtsv2.Handler.Ingest
       (
         ingestInstances
       , ingestInstance
       , ingestStart
       , ingestStop
       , ingestInstancesStats
       ) where

import Prelude

import Data.Either (hush)
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..), fromMaybe')
import Data.Newtype (unwrap, wrap)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Erl.Data.Binary (Binary)
import Erl.Data.Binary.IOData (IOData, fromBinary, toBinary, concat)
import Erl.Data.List (List, nil, (:))
import Erl.Data.Map (Map, fromFoldable, update)
import Erl.Data.Tuple (Tuple2, tuple2)
import Erl.Process.Raw as Raw
import Erl.Utils (Milliseconds)
import Erl.Utils as Timer
import Logger (spy)
import Rtsv2.Agents.IngestInstance as IngestInstance
import Rtsv2.Agents.IngestInstanceSup as IngestInstanceSup
import Rtsv2.Agents.IngestStats as IngestStats
import Rtsv2.Config as Config
import Rtsv2.Handler.MimeType as MimeType
import Rtsv2.Web.Bindings as Bindings
import Shared.LlnwApiTypes (StreamIngestProtocol(..), StreamPublish, StreamDetails)
import Shared.Stream (ShortName, StreamAndVariant(..), StreamId, StreamVariant, toVariant)
import Shared.Types.Agent.State as PublicState
import Shared.Types.Workflow.Metrics.Commmon (Stream)
import Shared.Utils (lazyCrashIfMissing)
import Simple.JSON (writeJSON)
import SpudGun (bodyToJSON)
import SpudGun as SpudGun
import Stetson (StetsonHandler)
import Stetson.Rest as Rest
import StetsonHelper (GenericStetsonGet, GenericStetsonGet2, genericGet, genericGet2, genericGetBy2)
import Unsafe.Coerce (unsafeCoerce)

ingestInstances :: StetsonHandler Unit
ingestInstances =
  Rest.handler (\req -> Rest.initResult req unit)
  # Rest.yeeha

ingestInstancesStats :: GenericStetsonGet2
ingestInstancesStats =
  genericGet2 nil ((MimeType.json getJson) : (MimeType.text getText) : nil)
  where
    getJson _ = do
      stats <- IngestStats.getStats
      pure $ writeJSON stats
    getText _ = do
      stats <- IngestStats.getStats
      pure $ stats_to_prometheus stats

data PrometheusMetricType = Counter
                          | Gauge
                          | Histogram
                          | Summary
                          | Untyped

type PrometheusMetric = { name :: String
                        , help :: String
                        , metricType :: PrometheusMetricType
                        }

newtype PrometheusTimestamp = PrometheusTimestamp IOData

newtype PrometheusLabels = PrometheusLabels IOData

type PrometheusMetricValue = { value :: String
                             , timestamp :: PrometheusTimestamp
                             , labels :: PrometheusLabels
                             }

type PrometheusPage = { metrics :: List PrometheusMetric
                      , metricsData ::  Map String (List PrometheusMetricValue)
                      }

metrics :: List PrometheusMetric
metrics = { name: "ingest_frame_count"
          , help: "The number of frames processed in this stream"
          , metricType: Counter
          } :
          { name: "ingest_frames_per_second"
          , help: "The number of frames per second measured in this stream"
          , metricType: Gauge
          } :
          { name: "ingest_frame_bitrate"
          , help: "The measured bitrate for this stream"
          , metricType: Gauge
          } :
          { name: "ingest_average_frame_size"
          , help: "The average size of frames in this stream"
          , metricType: Gauge
          } :
          nil

newPage :: List PrometheusMetric -> PrometheusPage
newPage pageMetrics =
  { metrics: pageMetrics
  , metricsData: fromFoldable $ (\{name} -> Tuple name nil) <$> metrics
  }

pageToString :: PrometheusPage -> String
pageToString page =
  let
    _ = spy "hello" page
  in
   ""

addMetric :: forall a. Show a => String -> a -> PrometheusLabels -> PrometheusTimestamp -> PrometheusPage -> PrometheusPage
addMetric name value labels timestamp page@{metricsData} =
  let
    metric = {value: show value, timestamp, labels}
  in
   page{metricsData = update (\v -> Just (metric : v)) name metricsData}

stats_to_prometheus :: PublicState.IngestStats List -> String
stats_to_prometheus stats =
  foldl (\page { timestamp
              , streamBitrateMetrics
              , frameFlowMeterMetrics
              } ->
         let
           prometheusTimestamp = PrometheusTimestamp $ toIOData $ show (unwrap timestamp)
         in
          foldl
          (\innerPage
            perStream@{ metrics: { frameCount
                                 , packetsPerSecond
                                 , bitrate
                                 , averagePacketSize }} ->
           let
             labels = labelsForStream(perStream)
           in
            innerPage
            # addMetric "ingest_frame_count" frameCount labels prometheusTimestamp
            # addMetric "ingest_frames_per_second" packetsPerSecond labels prometheusTimestamp
            # addMetric "ingest_bitrate" bitrate labels prometheusTimestamp
            # addMetric "ingest_average_frame_size" averagePacketSize labels prometheusTimestamp
          )
          page
          streamBitrateMetrics.perStreamMetrics
        )
  (newPage metrics)
  stats
  # pageToString
  where
    labelsForStream :: forall a. Stream a -> PrometheusLabels
    labelsForStream { streamId
                    , frameType
                    , profileName} =
      PrometheusLabels $ concat $ toIOData <$> "stream_id=\"" : show streamId : "\",frame_type=\"" : show frameType : "\",profile_name=\"" : profileName : "\"" : nil

    toIOData :: String -> IOData
    toIOData = fromBinary <<< stringToBinary

    toString :: IOData -> String
    toString = binaryToString <<< toBinary

    stringToBinary :: String -> Binary
    stringToBinary = unsafeCoerce

    binaryToString :: Binary -> String
    binaryToString = unsafeCoerce

stats_to_prometheus2 :: PublicState.IngestStats List -> String
stats_to_prometheus2 stats =
  foldl (\acc { timestamp
              , streamBitrateMetrics
              , frameFlowMeterMetrics
              } ->
         let
           time = toIOData $ show (unwrap timestamp)
         in
          streamBitrateMetricsToPrometheus streamBitrateMetrics time acc
          # frameFlowMetricsToPrometheus frameFlowMeterMetrics time
        )
        { frameCountAcc : mempty :: IOData
        , ppsAcc : mempty :: IOData
        , bitrateAcc : mempty :: IOData
        , averagePacketSizeAcc : mempty :: IOData
        , frameCount2Acc : mempty :: IOData
        , byteCountAcc : mempty :: IOData
        , lastDtsAcc : mempty :: IOData
        , lastPtsAcc : mempty :: IOData
        , lastCaptureMsAcc : mempty :: IOData
        , codecAcc : mempty :: IOData
        }
  stats
  # (\{ frameCountAcc
      , ppsAcc
      , bitrateAcc
      , averagePacketSizeAcc
      , frameCount2Acc
      , byteCountAcc
      , lastDtsAcc
      , lastPtsAcc
      , lastCaptureMsAcc
      , codecAcc
      } -> concat (frameCountHeader : frameCountAcc :
                   packetsPerSecondHeader : ppsAcc :
                   bitrateHeader : bitrateAcc :
                   averagePacketSizeHeader : averagePacketSizeAcc :
                   frameCount2Header : frameCountAcc :
                   byteCountHeader : byteCountAcc :
                   lastDtsHeader : lastDtsAcc :
                   lastPtsHeader : lastPtsAcc :
                   lastCaptureMsHeader : lastCaptureMsAcc :
                   codecHeader : codecAcc :
                   nil) )
  # toString
  where
    streamBitrateMetricsToPrometheus {perStreamMetrics} time acc =
      foldl
      (\innerAcc@{ frameCountAcc
                 , ppsAcc
                 , bitrateAcc
                 , averagePacketSizeAcc}
        perStream@{ metrics: { frameCount
                             , packetsPerSecond
                             , bitrate
                             , averagePacketSize }} ->
       let
         labels = labelsForStream(perStream)
         line2 = line labels time :: forall a. Show a => String -> a -> IOData
         frameCountLine    = line2 "ingest_frame_count" frameCount
         ppsLine           = line2 "ingest_frames_per_second" packetsPerSecond
         bitrateLine       = line2 "ingest_bitrate" bitrate
         avgPacketSizeLine = line2 "ingest_average_frame_size" averagePacketSize
       in
        innerAcc{ frameCountAcc = frameCountLine <> frameCountAcc
                , ppsAcc = ppsLine <> ppsAcc
                , bitrateAcc = bitrateLine <> bitrateAcc
                , averagePacketSizeAcc = avgPacketSizeLine <> averagePacketSizeAcc
                }
      )
      acc
      perStreamMetrics

    frameFlowMetricsToPrometheus {perStreamMetrics} time acc =
      foldl
      (\innerAcc@{ frameCount2Acc
                 , byteCountAcc
                 , lastDtsAcc
                 , lastPtsAcc
                 , lastCaptureMsAcc
                 , codecAcc }
        perStream@{ metrics: { frameCount
                             , byteCount
                             , lastDts
                             , lastPts
                             , lastCaptureMs
                             , codec }} ->
       let
         -- Lookup frame_count metric and add my value - with my labels and timestamp

         labels = labelsForStream(perStream)
         line2 = line labels time :: forall a. Show a => String -> a -> IOData
         frameCountLine    = line2 "ingest_frame_count2" frameCount
         byteCountLine    = line2 "ingest_byte_count" byteCount
         lastDtsLine    = line2 "ingest_last_dts" lastDts
         lastPtsLine    = line2 "ingest_last_pts" lastPts
         lastCaptureMsLine    = line2 "ingest_last_capture_ms" lastCaptureMs
         codecLine    = line2 "ingest_codec" codec
       in
        innerAcc{ frameCount2Acc = frameCountLine <> frameCount2Acc
                , byteCountAcc = byteCountLine <> byteCountAcc
                , lastDtsAcc = lastDtsLine <> lastDtsAcc
                , lastPtsAcc = lastPtsLine <> lastPtsAcc
                , lastCaptureMsAcc = lastCaptureMsLine <> lastCaptureMsAcc
                , codecAcc = codecLine <> codecAcc
                }
      )
      acc
      perStreamMetrics

    labelsForStream :: forall a. Stream a -> IOData
    labelsForStream { streamId
                    , frameType
                    , profileName} =
      concat $ toIOData <$> "stream_id=\"" : show streamId : "\",frame_type=\"" : show frameType : "\",profile_name=\"" : profileName : "\"" : nil

    line :: forall a. Show a => IOData -> IOData -> String -> a -> IOData
    line labels time name value =
      concat $ (toIOData name) : (toIOData "{") : labels : (toIOData "} ") : (toIOData (show value)) : (toIOData " ") : time : (toIOData "\n") : nil

    frameCountHeader = toIOData "\n# HELP ingest_frame_count The number of frames processed in this stream\n# TYPE ingest_frame_count counter\n"
    packetsPerSecondHeader = toIOData "\n# HELP ingest_frames_per_second The number of frames per second measured in this stream\n# TYPE ingest_frames_per_second gauge\n"
    bitrateHeader = toIOData "\n# HELP ingest_frame_bitrate The measured bitrate for this stream\n# TYPE ingest_frame_bitrate gauge\n"
    averagePacketSizeHeader = toIOData "\n# HELP ingest_average_frame_size The average size of frames in this stream\n# TYPE ingest_average_frame_size gauge\n"
    frameCount2Header = toIOData "\n# HELP\n# TYPE \n"
    byteCountHeader = toIOData "\n# HELP\n# TYPE \n"
    lastDtsHeader = toIOData "\n# HELP\n# TYPE \n"
    lastPtsHeader = toIOData "\n# HELP\n# TYPE \n"
    lastCaptureMsHeader = toIOData "\n# HELP\n# TYPE \n"
    codecHeader = toIOData "\n# HELP\n# TYPE \n"

    toIOData :: String -> IOData
    toIOData = fromBinary <<< stringToBinary

    toString :: IOData -> String
    toString = binaryToString <<< toBinary

    stringToBinary :: String -> Binary
    stringToBinary = unsafeCoerce

    binaryToString :: Binary -> String
    binaryToString = unsafeCoerce

ingestInstance :: GenericStetsonGet PublicState.Ingest
ingestInstance =
  genericGetBy2 Bindings.streamIdBindingLiteral Bindings.variantBindingLiteral getState
  where
    getState :: StreamId -> StreamVariant -> Effect PublicState.Ingest
    getState streamId variant = IngestInstance.getPublicState (StreamAndVariant streamId variant)

type IngestStartState = { shortName :: ShortName
                        , streamAndVariant :: StreamAndVariant
                        , streamDetails :: Maybe StreamDetails}

ingestStart :: StetsonHandler IngestStartState
ingestStart =
  Rest.handler (\req ->
                 let shortName = Bindings.shortName req
                     streamAndVariant = Bindings.streamAndVariant req
                 in
                 Rest.initResult req { shortName
                                     , streamAndVariant
                                     , streamDetails: Nothing})

  # Rest.serviceAvailable (\req state -> do
                            isAgentAvailable <- IngestInstanceSup.isAvailable
                            Rest.result isAgentAvailable req state)
  # Rest.resourceExists (\req state@{shortName, streamAndVariant} ->
                          let
                            apiBody :: StreamPublish
                            apiBody = { host: "172.16.171.5"
                                      , protocol: Rtmp
                                      , shortname: unwrap shortName
                                      , streamName: unwrap $ toVariant streamAndVariant
                                      , username: "user"}
                          in
                           do
                             {streamPublishUrl} <- Config.llnwApiConfig
                             restResult <- bodyToJSON <$> SpudGun.postJson (wrap streamPublishUrl) apiBody
                             let
                               streamDetails = hush $ restResult
                             Rest.result true req state{streamDetails = streamDetails}
                          )
  -- TODO - hideous spawn here, but ingestInstance needs to do a monitor... - ideally we sleep forever and kill it in ingestStop...
  # Rest.contentTypesProvided (\req state ->
                                  Rest.result (tuple2 "text/plain" (\req2 state2@{streamDetails, streamAndVariant} -> do
                                                                       pid <- Raw.spawn ((\_ -> Timer.sleep (wrap 10000))
                                                                                         { receive: Raw.receive
                                                                                         , receiveWithTimeout: Raw.receiveWithTimeout
                                                                                         })
                                                                       _ <- IngestInstanceSup.startIngest (fromMaybe' (lazyCrashIfMissing "stream_details missing") streamDetails) streamAndVariant pid
                                                                       Rest.result "ingestStarted" req2 state2
                                                                   ) : nil) req state)
  # Rest.yeeha


type IngestStopState = { streamAndVariant :: StreamAndVariant }

ingestStop :: StetsonHandler IngestStopState
ingestStop =
  Rest.handler (\req ->
                 let streamAndVariant = Bindings.streamAndVariant req
                 in
                 Rest.initResult req {streamAndVariant}
               )

  # Rest.serviceAvailable (\req state -> do
                            isAgentAvailable <- IngestInstanceSup.isAvailable
                            Rest.result isAgentAvailable req state)

  # Rest.resourceExists (\req state@{streamAndVariant} -> do
                            isActive <- IngestInstance.isActive streamAndVariant
                            Rest.result isActive req state
                        )
  # Rest.contentTypesProvided (\req state ->
                                Rest.result (tuple2 "text/plain" (\req2 state2 -> do
                                                                     _ <- IngestInstance.stopIngest state.streamAndVariant
                                                                     Rest.result "ingestStopped" req2 state2
                                                                 ) : nil) req state)
  # Rest.yeeha
