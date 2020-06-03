module Rtsv2.Handler.Health
       (
         healthCheck
       , vmMetrics
       ) where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Erl.Data.List (nil, (:))
import Rtsv2.Agents.IntraPoP as IntraPoP
import Rtsv2.Agents.TransPoP as TransPoP
import Rtsv2.Alerts as Alerts
import Rtsv2.Handler.MimeType as MimeType
import Rtsv2.Load as Load
import Rtsv2.NodeManager as NodeManager
import Rtsv2.PoPDefinition as PoPDefinition
import Shared.Rtsv2.JsonLd as JsonLd
import Shared.Rtsv2.Types (extractAddress)
import Simple.JSON as JSON
import Stetson (StetsonHandler)
import Stetson.Rest as Rest
import StetsonHelper (GetHandler, multiMimeResponse)

healthCheck :: StetsonHandler Unit
healthCheck =
  Rest.handler (\req -> Rest.initResult req unit)
  # Rest.contentTypesProvided (\req state -> Rest.result (MimeType.json jsonHandler : nil) req unit)
  # Rest.yeeha
  where
    jsonHandler req state = do
      thisServer <- PoPDefinition.getThisServer
      currentTransPoP <- IntraPoP.getCurrentTransPoPLeader
      intraPoPHealth <- IntraPoP.health
      transPoPHealth <- TransPoP.health
      load <- Load.getLoad
      nodeManager <- NodeManager.getState
      alerts <- Alerts.currentAlerts
      let
        health = { intraPoPHealth
                 , transPoPHealth
                 , load
                 , nodeManager
                 , currentTransPoP : extractAddress <$> currentTransPoP
                 , alerts
                 }
      node <- JsonLd.healthNode health thisServer

      Rest.result (JSON.writeJSON node) req state

foreign import vmMetricsImpl :: Effect String

vmMetrics  :: GetHandler String
vmMetrics = multiMimeResponse (MimeType.openmetrics (pure <<< identity) : nil) (Just <$> vmMetricsImpl)
