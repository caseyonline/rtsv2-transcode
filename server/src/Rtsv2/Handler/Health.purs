module Rtsv2.Handler.Health
       (
         healthCheck
       , vmMetrics
       ) where

import Prelude

import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (wrap)
import Effect (Effect)
import Erl.Data.List (nil, (:))
import Rtsv2.Agents.IntraPoP as IntraPoP
import Rtsv2.Agents.TransPoP as TransPoP
import Rtsv2.Handler.MimeType as MimeType
import Rtsv2.Load as Load
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
      currentTransPoP <- IntraPoP.getCurrentTransPoPLeader
      intraPoPHealth <- IntraPoP.health
      transPoPHealth <- TransPoP.health
      load <- Load.getLoad
      let
        result = {intraPoPHealth,
                  transPoPHealth,
                  load,
                  currentTransPoP : maybe (wrap "") extractAddress currentTransPoP}
      Rest.result (JSON.writeJSON result) req state

foreign import vmMetricsImpl :: Effect String

vmMetrics  :: GetHandler String
vmMetrics = multiMimeResponse (MimeType.openmetrics identity : nil) (Just <$> vmMetricsImpl)
