module Prometheus
       (
         newPage
       , addMetric
       , pageToString
       , toTimestamp
       , toLabels
       , PrometheusMetric
       , PrometheusMetricType(..)
       , PrometheusPage
       , class PrometheusValue, toValue
       , class PrometheusLabelValue, toLabelValue
       , IOLabelValue
       , IOLabels
       , IOTimestamp
       ) where

import Prelude

import Data.Foldable (foldl, intercalate)
import Data.Int (round)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..))
import Erl.Data.Binary (Binary)
import Erl.Data.Binary.IOData (IOData, concat, fromBinary, toBinary)
import Erl.Data.List (List, nil, (:))
import Erl.Data.Map (Map, fromFoldable, lookup, update)
import Shared.Common (Milliseconds)
import Unsafe.Coerce (unsafeCoerce)

class PrometheusValue a where
  toValue :: a -> IOData

instance pValueInt :: PrometheusValue Int where
  toValue = toIOData <<< show

instance pValueNumber :: PrometheusValue Number where
  toValue = toIOData <<< show

instance pValueMaybe :: PrometheusValue a => PrometheusValue (Maybe a) where
  toValue Nothing = toIOData "NaN"
  toValue (Just a) = toValue a

class PrometheusLabelValue a where
  toLabelValue :: a -> IOLabelValue

instance pLabelValueInt :: PrometheusLabelValue Int where
  toLabelValue = IOLabelValue <<< toIOData <<< show

instance pLabelValueString :: PrometheusLabelValue String where
  toLabelValue = IOLabelValue <<< toIOData

data PrometheusMetricType = Counter
                          | Gauge
                          | Histogram
                          | Summary
                          | Untyped

instance logablePrometheusMetricType :: PrometheusValue PrometheusMetricType where
  toValue Counter = toIOData "counter"
  toValue Gauge = toIOData "gauge"
  toValue Histogram = toIOData "histogram"
  toValue Summary = toIOData "summary"
  toValue Untyped = toIOData "untyped"

type PrometheusMetric = { name :: String
                        , help :: String
                        , metricType :: PrometheusMetricType
                        }

newtype PrometheusPage = PrometheusPage { metrics :: List PrometheusMetric
                                        , metricsData ::  Map String (List PrometheusMetricValue)
                                        }

newtype IOTimestamp = IOTimestamp IOData

newtype IOLabelValue = IOLabelValue IOData

newtype IOLabels = IOLabels IOData

newtype IOValue = IOValue IOData

type PrometheusMetricValue = { value :: IOValue
                             , timestamp :: IOTimestamp
                             , labels :: IOLabels
                             }

toTimestamp :: Milliseconds -> IOTimestamp
toTimestamp timestamp =
  IOTimestamp $ toValue (round $ unwrap timestamp)

toLabels :: List (Tuple String IOLabelValue) -> IOLabels
toLabels list =
  let
--    labels = (\(Tuple name value) -> concat $ toIOData <$> name : "=\"" : (toLabelValue value) : "\"" : nil) <$> list
    labels = (\(Tuple name (IOLabelValue value)) -> concat $ (toIOData name) : (toIOData "=\"") : value : (toIOData "\"") : nil) <$> list
  in
  IOLabels $ (intercalate (toIOData ",") labels)

newPage :: List PrometheusMetric -> PrometheusPage
newPage pageMetrics =
  PrometheusPage { metrics: pageMetrics
                 , metricsData: fromFoldable $ (\{name} -> Tuple name nil) <$> pageMetrics
                 }

pageToString :: PrometheusPage -> String
pageToString (PrometheusPage {metrics, metricsData}) =
  foldl (\acc {name, help, metricType} ->
          case lookup name metricsData of
            Nothing -> acc
            Just values ->
              let
                ioName = toIOData name
                header = concat $ (toIOData "# HELP ") : ioName : (toIOData " ") : (toIOData help) : (toIOData "\n") :
                                  (toIOData "# TYPE ") : ioName : (toIOData " ") : (toValue metricType) : (toIOData "\n") :
                                  nil

                lines = foldl (\lineAcc { value: (IOValue value)
                                        , timestamp: (IOTimestamp timestamp)
                                        , labels: (IOLabels labels)} ->
                                let
                                  line = concat $ ioName : (toIOData "{") : labels : (toIOData "} ") : value : (toIOData "\n") : nil -- (toIOData " ") : timestamp : (toIOData "\n") : nil
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

addMetric :: forall a. PrometheusValue a => String -> a -> IOLabels -> IOTimestamp -> PrometheusPage -> PrometheusPage
addMetric name value labels timestamp (PrometheusPage page@{metricsData}) =
  let
    metric = {value: IOValue (toValue value), timestamp, labels}
  in
   PrometheusPage page{metricsData = update (\v -> Just (metric : v)) name metricsData}

------------------------------------------------------------------------------
-- Internals
------------------------------------------------------------------------------
toIOData :: String -> IOData
toIOData = fromBinary <<< stringToBinary
  where
    stringToBinary :: String -> Binary
    stringToBinary = unsafeCoerce

toString :: IOData -> String
toString = binaryToString <<< toBinary
  where
    binaryToString :: Binary -> String
    binaryToString = unsafeCoerce
