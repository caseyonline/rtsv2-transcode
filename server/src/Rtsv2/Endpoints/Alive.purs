module Rtsv2.Endpoints.Alive
       (
         alive
       ) where

import Prelude

import Data.Maybe (fromMaybe)
import Erl.Data.List (nil, (:))
import Rtsv2.Endpoints.MimeType as MimeType
import Rtsv2.IntraPoPAgent as IntraPoPAgent
import Rtsv2.TransPoPAgent as TransPoPAgent
import Simple.JSON as JSON
import Stetson (StetsonHandler)
import Stetson.Rest as Rest

alive :: StetsonHandler Unit
alive =
  Rest.handler (\req -> Rest.initResult req unit)
  # Rest.contentTypesProvided (\req state -> Rest.result (MimeType.json jsonHandler : nil) req unit)
  # Rest.yeeha
  where
    jsonHandler req state = do
      currentTransPoP <- IntraPoPAgent.currentTransPoPLeader
      intraPoPHealth <- IntraPoPAgent.health
      transPoPHealth <- TransPoPAgent.health
      let
        result = {intraPoPHealth,
                  transPoPHealth,
                  currentTransPoP : fromMaybe "" currentTransPoP}
      Rest.result (JSON.writeJSON result) req state
