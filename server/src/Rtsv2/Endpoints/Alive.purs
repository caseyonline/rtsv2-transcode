module Rtsv2.Endpoints.Alive
       (
         alive
       ) where

import Prelude

import Erl.Data.List (nil, (:))
import Rtsv2.Endpoints.MimeType as MimeType
import Rtsv2.IntraPoPAgent as IntraPoPAgent
import Rtsv2.TransPoPAgent as TransPoPAgent
import Stetson (StetsonHandler)
import Stetson.Rest as Rest
import Simple.JSON as JSON

alive :: StetsonHandler Unit
alive =
  Rest.handler (\req -> Rest.initResult req unit)
  # Rest.contentTypesProvided (\req state -> Rest.result (MimeType.json jsonHandler : nil) req unit)
  # Rest.yeeha
  where
    jsonHandler req state = do
      intraPoPHealth <- IntraPoPAgent.health
      transPoPHealth <- TransPoPAgent.health
      let
        result = {intraPoPHealth,
                  transPoPHealth}
      Rest.result (JSON.writeJSON result) req state
