module Rtsv2.Handler.Health
       (
         healthCheck
       ) where

import Prelude

import Data.Maybe (maybe)
import Data.Newtype (wrap)
import Erl.Data.List (nil, (:))
import Rtsv2.Agents.IntraPoP as IntraPoP
import Rtsv2.Agents.TransPoP as TransPoP
import Rtsv2.Handler.MimeType as MimeType
import Shared.Types (extractAddress)
import Simple.JSON as JSON
import Stetson (StetsonHandler)
import Stetson.Rest as Rest

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
