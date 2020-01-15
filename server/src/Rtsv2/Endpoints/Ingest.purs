module Rtsv2.Endpoints.Ingest
       (
         ingestStart
       , ingestStop
       ) where

import Prelude

import Data.Array ((!!))
import Data.Bifunctor (lmap)
import Data.Either (hush)
import Data.List.NonEmpty (singleton)
import Data.Maybe (Maybe(..), fromMaybe')
import Data.String (Pattern(..), split)
import Erl.Atom (atom)
import Erl.Cowboy.Req (binding)
import Erl.Data.List (nil, (:))
import Erl.Data.Tuple (tuple2)
import Foreign (ForeignError(..))
import Logger (spy)
import Rtsv2.Agents.IngestInstance as IngestInstance
import Rtsv2.Agents.IngestInstanceSup as IngestInstanceSup
import Rtsv2.Config as Config
import Shared.LlnwApiTypes (StreamIngestProtocol(..), StreamPublish, StreamDetails)
import Shared.Stream (StreamVariantId(..))
import Shared.Utils (lazyCrashIfMissing)
import Simple.JSON as JSON
import SpudGun as SpudGun
import Stetson (StetsonHandler)
import Stetson.Rest as Rest

type IngestStartState = { shortName :: String
                        , streamId :: String
                        , variantId :: String
                        , streamVariantId :: StreamVariantId
                        , streamDetails :: Maybe StreamDetails}

ingestStart :: StetsonHandler IngestStartState
ingestStart =
  Rest.handler (\req ->
                 let shortName = spy "init" (fromMaybe' (lazyCrashIfMissing "short_name binding missing") $ binding (atom "short_name") req)
                     variantId = fromMaybe' (lazyCrashIfMissing "variant_id binding missing") $ binding (atom "variant_id") req
                     streamId = fromMaybe' (lazyCrashIfMissing "variant_id badly formed") $ (split (Pattern "_") variantId) !! 0
                 in
                 Rest.initResult req { shortName
                                     , streamId
                                     , variantId
                                     , streamVariantId: StreamVariantId streamId variantId
                                     , streamDetails: Nothing})

  # Rest.serviceAvailable (\req state -> do
                            isAgentAvailable <- IngestInstanceSup.isAvailable
                            Rest.result isAgentAvailable req state)
  # Rest.resourceExists (\req state@{shortName, streamId, variantId} ->
                          let
                            apiBody :: StreamPublish
                            apiBody = {host: "172.16.171.5"
                                      , protocol: Rtmp
                                      , shortname: shortName
                                      , streamName: variantId
                                      , username: "user"}
                          in
                           do
                             {streamPublishUrl} <- Config.llnwApiConfig
                             restResult <- SpudGun.post streamPublishUrl (JSON.writeJSON apiBody)
                             let
                               streamDetails = hush $ JSON.readJSON =<< lmap (\s -> (singleton (ForeignError s))) restResult
                             Rest.result true req state{streamDetails = streamDetails}
                          )
  -- TODO - would conflict if stream exists, but this won't be REST once media plugged...
  -- # Rest.isConflict (\req state@{streamVariantId} -> do

  --                           isAvailable <- isIngestActive streamVariantId
  --                           Rest.result isAvailable req $ spy "state" state
  --                         )
  # Rest.contentTypesProvided (\req state -> 
                                  Rest.result (tuple2 "text/plain" (\req2 state2@{streamDetails} -> do
                                                                       _ <- IngestInstanceSup.startIngest (fromMaybe' (lazyCrashIfMissing "stream_details missing") streamDetails) state.streamVariantId
                                                                       Rest.result "ingestStarted" req2 state2
                                                                   ) : nil) req state)
  # Rest.yeeha


type IngestStopState = { streamVariantId :: StreamVariantId }

ingestStop :: StetsonHandler IngestStopState
ingestStop =
  Rest.handler (\req ->
                 let variantId = fromMaybe' (lazyCrashIfMissing "variant_id binding missing") $ binding (atom "variant_id") req
                     streamId = fromMaybe' (lazyCrashIfMissing "variant_id badly formed") $ (split (Pattern "_") variantId) !! 0
                 in
                 Rest.initResult req {streamVariantId: StreamVariantId streamId variantId})

  # Rest.serviceAvailable (\req state -> do
                            isAgentAvailable <- IngestInstanceSup.isAvailable
                            Rest.result isAgentAvailable req state)

  # Rest.resourceExists (\req state@{streamVariantId} -> do
                            isActive <- IngestInstance.isActive streamVariantId
                            Rest.result isActive req state
                          )
  # Rest.contentTypesProvided (\req state ->
                                Rest.result (tuple2 "text/plain" (\req2 state2 -> do
                                                                     _ <- IngestInstance.stopIngest state.streamVariantId
                                                                     Rest.result "ingestStopped" req2 state2
                                                                 ) : nil) req state)
  # Rest.yeeha
