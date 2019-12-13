module Rtsv2.Endpoints.Ingest
       (
         ingestStart
       , ingestStop
       ) where

import Prelude

import Data.Maybe (fromMaybe')
import Erl.Atom (atom)
import Erl.Cowboy.Req (binding)
import Erl.Data.List (nil, (:))
import Erl.Data.Tuple (tuple2)
import Rtsv2.IngestAgent as IngestAgent
import Rtsv2.IngestAgentInstanceSup as IngestAgentInstanceSup
import Shared.Stream (StreamVariantId(..))
import Shared.Utils (lazyCrashIfMissing)
import Stetson (StetsonHandler)
import Stetson.Rest as Rest

type IngestStartState = { streamVariantId :: StreamVariantId }

ingestStart :: StetsonHandler IngestStartState
ingestStart =
  Rest.handler (\req ->
                 let streamId = fromMaybe' (lazyCrashIfMissing "stream_id binding missing") $ binding (atom "stream_id") req
                     variantId = fromMaybe' (lazyCrashIfMissing "variant_id binding missing") $ binding (atom "variant_id") req
                 in
                 Rest.initResult req {streamVariantId: StreamVariantId streamId variantId})

  # Rest.serviceAvailable (\req state -> do
                            isAgentAvailable <- IngestAgentInstanceSup.isAvailable
                            Rest.result isAgentAvailable req state)
  -- TODO - would conflict if stream exists, but this won't be REST once media plugged...
  -- # Rest.isConflict (\req state@{streamVariantId} -> do

  --                           isAvailable <- isIngestActive streamVariantId
  --                           Rest.result isAvailable req $ spy "state" state
  --                         )
  # Rest.contentTypesProvided (\req state -> do
                                  _ <- IngestAgentInstanceSup.startIngest state.streamVariantId
                                  Rest.result (tuple2 "text/plain" (\req2 state2 -> Rest.result "ingestStarted" req2 state2) : nil) req state)
  # Rest.yeeha


type IngestStopState = { streamVariantId :: StreamVariantId }

ingestStop :: StetsonHandler IngestStopState
ingestStop =
  Rest.handler (\req ->
                 let streamId = fromMaybe' (lazyCrashIfMissing "stream_id binding missing") $ binding (atom "stream_id") req
                     variantId = fromMaybe' (lazyCrashIfMissing "variant_id binding missing") $ binding (atom "variant_id") req
                 in
                 Rest.initResult req {streamVariantId: StreamVariantId streamId variantId})

  # Rest.serviceAvailable (\req state -> do
                            isAgentAvailable <- IngestAgentInstanceSup.isAvailable
                            Rest.result isAgentAvailable req state)

  # Rest.resourceExists (\req state@{streamVariantId} -> do
                            isActive <- IngestAgent.isActive streamVariantId
                            Rest.result isActive req state
                          )
  # Rest.contentTypesProvided (\req state ->
                                Rest.result (tuple2 "text/plain" (\req2 state2 -> do
                                                                     _ <- IngestAgent.stopIngest state.streamVariantId
                                                                     Rest.result "ingestStopped" req2 state2
                                                                 ) : nil) req state)
  # Rest.yeeha
