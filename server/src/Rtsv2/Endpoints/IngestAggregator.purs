module Rtsv2.Endpoints.IngestAggregator
       ( ingestAggregators
       )
       where

import Prelude

import Erl.Data.List (nil, (:))
import Rtsv2.Agents.IngestAggregatorInstanceSup as IngestAggregatorInstanceSup
import Rtsv2.Endpoints.MimeType as MimeType
import Simple.JSON as JSON
import Stetson (HttpMethod(..), StetsonHandler)
import Stetson.Rest as Rest

type IngestAggregatorsState = { }
ingestAggregators :: StetsonHandler IngestAggregatorsState
ingestAggregators =
  Rest.handler (\req -> Rest.initResult req { })

  # Rest.serviceAvailable (\req state -> do
                              isAgentAvailable <- IngestAggregatorInstanceSup.isAvailable
                              Rest.result isAgentAvailable req state)

  # Rest.allowedMethods (Rest.result (GET : nil))
  # Rest.contentTypesProvided (\req state -> Rest.result (MimeType.json jsonHandler : nil) req state)
  # Rest.yeeha
  where
    jsonHandler req state = 
--      currentLoad <- Load.load
      let
        result = 1.0
      in
       Rest.result (JSON.writeJSON result) req state
