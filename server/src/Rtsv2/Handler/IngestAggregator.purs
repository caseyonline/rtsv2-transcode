module Rtsv2.Handler.IngestAggregator
       ( ingestAggregator
       , ingestAggregators
       , ingestAggregatorsActiveIngest
       )
       where

import Prelude

import Data.Either (hush)
import Data.Maybe (Maybe(..), fromMaybe', isNothing)
import Erl.Cowboy.Req (method)
import Erl.Data.List (nil, (:))
import Erl.Data.Tuple (tuple2)
import Rtsv2.Agents.IngestAggregatorInstance as IngestAggregatorInstance
import Rtsv2.Agents.IngestAggregatorInstanceSup as IngestAggregatorInstanceSup
import Rtsv2.Web.Bindings as Bindings
import Shared.LlnwApiTypes (StreamDetails)
import Shared.Stream (StreamAndVariant(StreamAndVariant), toStreamId)
import Shared.Types (ServerAddress)
import Shared.Types.Agent.State as PublicState
import Shared.Utils (lazyCrashIfMissing)
import Simple.JSON (readJSON)
import Stetson (HttpMethod(..), StetsonHandler)
import Stetson.Rest as Rest
import StetsonHelper (GenericStetsonGetByStreamId, GenericStetsonHandler, allBody, binaryToString, genericGetByStreamId, genericPost)

ingestAggregator :: GenericStetsonGetByStreamId PublicState.IngestAggregator
ingestAggregator = genericGetByStreamId IngestAggregatorInstance.getState

ingestAggregators :: GenericStetsonHandler StreamDetails
ingestAggregators = genericPost IngestAggregatorInstanceSup.startAggregator


type IngestAggregatorsActiveIngestState = { streamAndVariant :: StreamAndVariant
                                          , serverAddress :: Maybe ServerAddress
                                          }
ingestAggregatorsActiveIngest :: StetsonHandler IngestAggregatorsActiveIngestState
ingestAggregatorsActiveIngest =
  Rest.handler (\req ->
                 let
                   streamId = Bindings.streamId req
                   variant = Bindings.variant req
                 in
                  Rest.initResult req {streamAndVariant: StreamAndVariant streamId variant
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
