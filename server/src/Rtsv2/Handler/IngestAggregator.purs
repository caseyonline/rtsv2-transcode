module Rtsv2.Handler.IngestAggregator
       ( ingestAggregator
       , ingestAggregators
       , ingestAggregatorsActiveIngest
       )
       where

import Prelude

import Data.Either (hush)
import Data.Maybe (Maybe(..), fromMaybe', isNothing)
import Data.Newtype (wrap)
import Erl.Atom (atom)
import Erl.Cowboy.Req (binding, method)
import Erl.Data.List (nil, (:))
import Erl.Data.Tuple (tuple2)
import PintoHelper (GenericHandlerState, GenericStatusState, allBody, binaryToString, genericPost, genericStatus)
import Rtsv2.Agents.IngestAggregatorInstance as IngestAggregatorInstance
import Rtsv2.Agents.IngestAggregatorInstanceSup as IngestAggregatorInstanceSup
import Shared.LlnwApiTypes (StreamDetails)
import Shared.Stream (StreamAndVariant(StreamAndVariant), toStreamId)
import Shared.Types (ServerAddress)
import Shared.Types.Agent.State as PublicState
import Shared.Utils (lazyCrashIfMissing)
import Simple.JSON (readJSON)
import Stetson (HttpMethod(..), StetsonHandler)
import Stetson.Rest as Rest

ingestAggregator :: StetsonHandler (GenericStatusState PublicState.IngestAggregator)
ingestAggregator = genericStatus IngestAggregatorInstance.getState

ingestAggregators ::StetsonHandler (GenericHandlerState StreamDetails)
ingestAggregators = genericPost IngestAggregatorInstanceSup.startAggregator


type IngestAggregatorsActiveIngestState = { streamAndVariant :: StreamAndVariant
                                          , serverAddress :: Maybe ServerAddress
                                          }
ingestAggregatorsActiveIngest :: StetsonHandler IngestAggregatorsActiveIngestState
ingestAggregatorsActiveIngest =
  Rest.handler (\req ->
                 let
                   streamIdStr = fromMaybe' (lazyCrashIfMissing "stream_id binding missing") $ binding (atom "stream_id") req
                   variantIdStr = fromMaybe' (lazyCrashIfMissing "variant binding missing") $ binding (atom "variant_id") req
                 in
                  Rest.initResult req {streamAndVariant: StreamAndVariant (wrap streamIdStr) (wrap variantIdStr)
                                      , serverAddress: Nothing})
  # Rest.serviceAvailable (\req state -> do
                              isAgentAvailable <- IngestAggregatorInstanceSup.isAvailable
                              Rest.result isAgentAvailable req state)
  # Rest.allowedMethods (Rest.result (DELETE : POST : nil))
  # Rest.resourceExists (\req state@{streamAndVariant} -> do
                          isAvailable <- IngestAggregatorInstance.isAvailable (toStreamId streamAndVariant)
                          Rest.result isAvailable req state
                        )
  # Rest.malformedRequest (\req state ->
                            case method req of
                              "DELETE" ->
                                Rest.result false req state
                              "POST" ->
                                do
                                  body <- allBody req mempty
                                  let
                                    maybeServerAddress :: Maybe ServerAddress
                                    maybeServerAddress = hush $ readJSON $ binaryToString body
                                  Rest.result (isNothing maybeServerAddress) req state{serverAddress = maybeServerAddress}
                              _ ->
                                Rest.result false req state

                          )
  # Rest.contentTypesAccepted (\req state ->
                                Rest.result ((tuple2 "application/json" (\req2 state2@{ streamAndVariant
                                                                                      , serverAddress: maybeServerAddress} ->
                                                                          let
                                                                            serverAddress = fromMaybe' (lazyCrashIfMissing "server_address is nothing") maybeServerAddress
                                                                          in
                                                                            do
                                                                              _ <- IngestAggregatorInstance.addRemoteVariant streamAndVariant serverAddress
                                                                              Rest.result true req2 state2
                                                                        )) : nil)
                                req state
                              )


  # Rest.previouslyExisted (Rest.result false)

  # Rest.allowMissingPost (Rest.result false)

  # Rest.deleteResource (\req state@{streamAndVariant} -> do
                            _ <- IngestAggregatorInstance.removeVariant streamAndVariant
                            Rest.result true req state
                        )

  # Rest.contentTypesProvided (\req state -> Rest.result (tuple2 "application/json" (Rest.result ""): nil) req state)

  # Rest.yeeha
