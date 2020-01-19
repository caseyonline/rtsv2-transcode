module Rtsv2.Handler.Ingest
       (
         ingestStart
       , ingestStop
       ) where

import Prelude

import Data.Array ((!!))
import Data.Either (hush)
import Data.Maybe (Maybe(..), fromMaybe')
import Data.Newtype (unwrap, wrap)
import Data.String (Pattern(..), split)
import Erl.Atom (atom)
import Erl.Cowboy.Req (binding)
import Erl.Data.List (nil, (:))
import Erl.Data.Tuple (tuple2)
import Logger (spy)
import Rtsv2.Agents.IngestInstance as IngestInstance
import Rtsv2.Agents.IngestInstanceSup as IngestInstanceSup
import Rtsv2.Config as Config
import Shared.LlnwApiTypes (StreamIngestProtocol(..), StreamPublish, StreamDetails)
import Shared.Stream (StreamAndVariant(..), toVariant)
import Shared.Utils (lazyCrashIfMissing)
import Simple.JSON as JSON
import SpudGun (bodyToJSON)
import SpudGun as SpudGun
import Stetson (StetsonHandler)
import Stetson.Rest as Rest

type IngestStartState = { shortName :: String
                        , streamAndVariant :: StreamAndVariant
                        , streamDetails :: Maybe StreamDetails}

ingestStart :: StetsonHandler IngestStartState
ingestStart =
  Rest.handler (\req ->
                 let shortName = spy "init" (fromMaybe' (lazyCrashIfMissing "short_name binding missing") $ binding (atom "short_name") req)
                     variantId = fromMaybe' (lazyCrashIfMissing "variant_id binding missing") $ binding (atom "variant_id") req
                     streamId = fromMaybe' (lazyCrashIfMissing "variant_id badly formed") $ (split (Pattern "_") variantId) !! 0
                 in
                 Rest.initResult req { shortName
                                     , streamAndVariant: StreamAndVariant (wrap streamId) (wrap variantId)
                                     , streamDetails: Nothing})

  # Rest.serviceAvailable (\req state -> do
                            isAgentAvailable <- IngestInstanceSup.isAvailable
                            Rest.result isAgentAvailable req state)
  # Rest.resourceExists (\req state@{shortName, streamAndVariant} ->
                          let
                            apiBody :: StreamPublish
                            apiBody = {host: "172.16.171.5"
                                      , protocol: Rtmp
                                      , shortname: shortName
                                      , streamName: unwrap $ toVariant streamAndVariant
                                      , username: "user"}
                          in
                           do
                             {streamPublishUrl} <- Config.llnwApiConfig
                             restResult <- bodyToJSON <$> SpudGun.postJson (wrap streamPublishUrl) apiBody
                             let
                               streamDetails = hush $ restResult
                             Rest.result true req state{streamDetails = streamDetails}
                          )
  -- TODO - would conflict if stream exists, but this won't be REST once media plugged...
  -- # Rest.isConflict (\req state@{streamVariant} -> do

  --                           isAvailable <- isIngestActive streamVariant
  --                           Rest.result isAvailable req $ spy "state" state
  --                         )
  # Rest.contentTypesProvided (\req state ->
                                  Rest.result (tuple2 "text/plain" (\req2 state2@{streamDetails, streamAndVariant} -> do
                                                                       _ <- IngestInstanceSup.startIngest (fromMaybe' (lazyCrashIfMissing "stream_details missing") streamDetails) streamAndVariant
                                                                       Rest.result "ingestStarted" req2 state2
                                                                   ) : nil) req state)
  # Rest.yeeha


type IngestStopState = { streamVariant :: StreamAndVariant }

ingestStop :: StetsonHandler IngestStopState
ingestStop =
  Rest.handler (\req ->
                 let variantId = fromMaybe' (lazyCrashIfMissing "variant_id binding missing") $ binding (atom "variant_id") req
                     streamId = fromMaybe' (lazyCrashIfMissing "variant_id badly formed") $ (split (Pattern "_") variantId) !! 0
                 in
                 Rest.initResult req {streamVariant: StreamAndVariant (wrap streamId) (wrap variantId)})

  # Rest.serviceAvailable (\req state -> do
                            isAgentAvailable <- IngestInstanceSup.isAvailable
                            Rest.result isAgentAvailable req state)

  # Rest.resourceExists (\req state@{streamVariant} -> do
                            isActive <- IngestInstance.isActive streamVariant
                            Rest.result isActive req state
                          )
  # Rest.contentTypesProvided (\req state ->
                                Rest.result (tuple2 "text/plain" (\req2 state2 -> do
                                                                     _ <- IngestInstance.stopIngest state.streamVariant
                                                                     Rest.result "ingestStopped" req2 state2
                                                                 ) : nil) req state)
  # Rest.yeeha
