module Rtsv2.Endpoints.IngestAggregator
       ( ingestAggregator
       )
       where

import Prelude

import Data.Maybe (fromMaybe')
import Erl.Atom (atom)
import Erl.Cowboy.Req (binding)
import Erl.Data.List (nil, (:))
import Rtsv2.Agents.IngestAggregatorInstance as IngestAggregatorInstance
import Rtsv2.Agents.IngestAggregatorInstanceSup as IngestAggregatorInstanceSup
import Rtsv2.Endpoints.MimeType as MimeType
import Shared.Stream (StreamId(..))
import Shared.Utils (lazyCrashIfMissing)
import Simple.JSON as JSON
import Stetson (HttpMethod(..), StetsonHandler)
import Stetson.Rest as Rest

type IngestAggregatorState = { streamId :: StreamId}
ingestAggregator :: StetsonHandler IngestAggregatorState
ingestAggregator =
  Rest.handler (\req ->
                 let streamId = StreamId $ fromMaybe' (lazyCrashIfMissing "stream_id binding missing") $ binding (atom "stream_id") req
                 in
                  Rest.initResult req {streamId})

  # Rest.serviceAvailable (\req state -> do
                              isAgentAvailable <- IngestAggregatorInstanceSup.isAvailable
                              Rest.result isAgentAvailable req state)
  # Rest.resourceExists (\req state@{streamId} -> do
                          isAvailable <- IngestAggregatorInstance.isAvailable streamId
                          Rest.result isAvailable req state
                        )
  # Rest.allowedMethods (Rest.result (GET : nil))
  # Rest.contentTypesProvided (\req state -> Rest.result (MimeType.json jsonHandler : nil) req state)
  # Rest.yeeha
  where
    jsonHandler req state@{streamId} = do
      aggregatorState <- IngestAggregatorInstance.getState streamId
      Rest.result (JSON.writeJSON aggregatorState) req state
