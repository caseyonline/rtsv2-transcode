module Rtsv2.Handler.Health
       (
         healthCheck
       , vmMetrics
       ) where

import Prelude

import Data.Maybe (maybe)
import Data.Newtype (wrap)
import Effect (Effect)
import Erl.Data.List (nil, (:))
import Rtsv2.Agents.IntraPoP as IntraPoP
import Rtsv2.Agents.TransPoP as TransPoP
import Rtsv2.Handler.MimeType as MimeType
import Shared.Types (extractAddress)
import Simple.JSON as JSON
import Stetson (StetsonHandler)
import Stetson.Rest as Rest
import StetsonHelper (GenericStetsonGet2, genericGet2)

healthCheck :: StetsonHandler Unit
healthCheck =
  Rest.handler (\req -> Rest.initResult req unit)
  # Rest.contentTypesProvided (\req state -> Rest.result (MimeType.json jsonHandler : nil) req unit)
  # Rest.yeeha
  where
    jsonHandler req state = do
      currentTransPoP <- IntraPoP.getCurrentTransPoPLeader
      intraPoPHealth <- IntraPoP.health
      transPoPHealth <- TransPoP.health
      let
        result = {intraPoPHealth,
                  transPoPHealth,
                  currentTransPoP : maybe (wrap "") extractAddress currentTransPoP}
      Rest.result (JSON.writeJSON result) req state

foreign import vmMetricsImpl :: Effect String

vmMetrics  :: GenericStetsonGet2
vmMetrics =
  genericGet2 nil ((MimeType.openmetrics getText) : nil)
  where
    getText _ = do
      metrics <- vmMetricsImpl
      pure $ metrics
