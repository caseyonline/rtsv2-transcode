module Rtsv2.Handler.Ingest
       (
         ingestStart
       , ingestStop
       ) where

import Prelude

import Data.Either (hush)
import Data.Maybe (Maybe(..), fromMaybe')
import Data.Newtype (unwrap, wrap)
import Erl.Data.List (nil, (:))
import Erl.Data.Tuple (tuple2)
import Rtsv2.Agents.IngestInstance as IngestInstance
import Rtsv2.Agents.IngestInstanceSup as IngestInstanceSup
import Rtsv2.Config as Config
import Rtsv2.Web.Bindings as Bindings
import Shared.LlnwApiTypes (StreamIngestProtocol(..), StreamPublish, StreamDetails)
import Shared.Stream (ShortName, StreamAndVariant, toVariant)
import Shared.Utils (lazyCrashIfMissing)
import SpudGun (bodyToJSON)
import SpudGun as SpudGun
import Stetson (StetsonHandler)
import Stetson.Rest as Rest

type IngestStartState = { shortName :: ShortName
                        , streamAndVariant :: StreamAndVariant
                        , streamDetails :: Maybe StreamDetails}

ingestStart :: StetsonHandler IngestStartState
ingestStart =
  Rest.handler (\req ->
                 let shortName = Bindings.shortName req
                     streamAndVariant = Bindings.streamAndVariant req
                 in
                 Rest.initResult req { shortName
                                     , streamAndVariant
                                     , streamDetails: Nothing})

  # Rest.serviceAvailable (\req state -> do
                            isAgentAvailable <- IngestInstanceSup.isAvailable
                            Rest.result isAgentAvailable req state)
  # Rest.resourceExists (\req state@{shortName, streamAndVariant} ->
                          let
                            apiBody :: StreamPublish
                            apiBody = { host: "172.16.171.5"
                                      , protocol: Rtmp
                                      , shortname: unwrap shortName
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


type IngestStopState = { streamAndVariant :: StreamAndVariant }

ingestStop :: StetsonHandler IngestStopState
ingestStop =
  Rest.handler (\req ->
                 let streamAndVariant = Bindings.streamAndVariant req
                 in
                 Rest.initResult req {streamAndVariant}
               )

  # Rest.serviceAvailable (\req state -> do
                            isAgentAvailable <- IngestInstanceSup.isAvailable
                            Rest.result isAgentAvailable req state)

  # Rest.resourceExists (\req state@{streamAndVariant} -> do
                            isActive <- IngestInstance.isActive streamAndVariant
                            Rest.result isActive req state
                        )
  # Rest.contentTypesProvided (\req state ->
                                Rest.result (tuple2 "text/plain" (\req2 state2 -> do
                                                                     _ <- IngestInstance.stopIngest state.streamAndVariant
                                                                     Rest.result "ingestStopped" req2 state2
                                                                 ) : nil) req state)
  # Rest.yeeha
