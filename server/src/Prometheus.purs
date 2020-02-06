module Prometheus
       (
         newPage
       , addMetric
       , pageToString
       , toTimestamp
       , toLabels
       , PrometheusMetric
       , PrometheusMetricType(..)
       , PrometheusTimestamp
       , PrometheusLabels
       ) where

import Prelude

import Data.Foldable (foldl, intercalate)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..))
import Erl.Data.Binary (Binary)
import Erl.Data.Binary.IOData (IOData, concat, fromBinary, toBinary)
import Erl.Data.List (List, nil, (:))
import Erl.Data.Map (Map, fromFoldable, lookup, update)
import Shared.Types (Milliseconds)
import Unsafe.Coerce (unsafeCoerce)

data PrometheusMetricType = Counter
                          | Gauge
                          | Histogram
                          | Summary
                          | Untyped
instance showPrometheusMetricType :: Show PrometheusMetricType where
  show Counter = "counter"
  show Gauge = "gauge"
  show Histogram = "histogram"
  show Summary = "summary"
  show Untyped = "untyped"

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

toIOData :: String -> IOData
toIOData = fromBinary <<< stringToBinary
  where
    stringToBinary :: String -> Binary
    stringToBinary = unsafeCoerce

toTimestamp :: Milliseconds -> PrometheusTimestamp
toTimestamp timestamp =
  PrometheusTimestamp $ toIOData $ show (unwrap timestamp)

toLabels :: List (Tuple String String) -> PrometheusLabels
toLabels list =
  let
    labels = (\(Tuple name value) -> concat $ toIOData <$> name : "=\"" : value : "\"" : nil) <$> list
  in
  PrometheusLabels $ (intercalate (toIOData ",") labels)

newPage :: List PrometheusMetric -> PrometheusPage
newPage pageMetrics =
  { metrics: pageMetrics
  , metricsData: fromFoldable $ (\{name} -> Tuple name nil) <$> pageMetrics
  }

pageToString :: PrometheusPage -> String
pageToString {metrics, metricsData} =
  foldl (\acc {name, help, metricType} ->
          case lookup name metricsData of
            Nothing -> acc
            Just values ->
              let
                ioName = toIOData name
                header = concat $ (toIOData "\n") :
                                  (toIOData "# HELP ") : ioName : (toIOData " ") : (toIOData help) : (toIOData "\n") :
                                  (toIOData "# TYPE ") : ioName : (toIOData " ") : (toIOData (show metricType)) : (toIOData "\n") :
                                  nil

                lines = foldl (\lineAcc {value, timestamp: (PrometheusTimestamp timestamp), labels: (PrometheusLabels labels)} ->
                                let
                                  line = concat $ ioName : (toIOData "{") : labels : (toIOData "} ") : (toIOData value) : (toIOData " ") : timestamp : (toIOData "\n") : nil
                                in
                                 line <> lineAcc
                              )
                        acc
                        values
              in
               header <> lines
        )
        (mempty :: IOData)
        metrics
  # toString
  where
    toIOData :: String -> IOData
    toIOData = fromBinary <<< stringToBinary

    toString :: IOData -> String
    toString = binaryToString <<< toBinary

    stringToBinary :: String -> Binary
    stringToBinary = unsafeCoerce

    binaryToString :: Binary -> String
    binaryToString = unsafeCoerce


addMetric :: forall a. Show a => String -> a -> PrometheusLabels -> PrometheusTimestamp -> PrometheusPage -> PrometheusPage
addMetric name value labels timestamp page@{metricsData} =
  let
    metric = {value: show value, timestamp, labels}
  in
   page{metricsData = update (\v -> Just (metric : v)) name metricsData}
