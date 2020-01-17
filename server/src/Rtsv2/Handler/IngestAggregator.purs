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
import Effect (Effect)
import Erl.Atom (atom)
import Erl.Cowboy.Req (ReadBodyResult(..), Req, binding, method, readBody)
import Erl.Data.Binary (Binary)
import Erl.Data.Binary.IOData (IOData, fromBinary, toBinary)
import Erl.Data.List (nil, (:))
import Erl.Data.Tuple (tuple2)
import Rtsv2.Agents.IngestAggregatorInstance as IngestAggregatorInstance
import Rtsv2.Agents.IngestAggregatorInstanceSup as IngestAggregatorInstanceSup
import Rtsv2.Handler.MimeType as MimeType
import Shared.LlnwApiTypes (StreamDetails)
import Shared.Stream (StreamAndVariant(StreamAndVariant), StreamId(..), toStreamId)
import Shared.Types (ServerAddress)
import Shared.Utils (lazyCrashIfMissing)
import Simple.JSON (readJSON)
import Simple.JSON as JSON
import Stetson (HttpMethod(..), StetsonHandler)
import Stetson.Rest as Rest
import Unsafe.Coerce (unsafeCoerce)

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

type IngestAggregatorsState = { streamDetails :: Maybe StreamDetails}
ingestAggregators :: StetsonHandler IngestAggregatorsState
ingestAggregators =
  Rest.handler (\req -> Rest.initResult req {streamDetails: Nothing})
  # Rest.serviceAvailable (\req state -> do
                              isAgentAvailable <- IngestAggregatorInstanceSup.isAvailable
                              Rest.result isAgentAvailable req state)
  # Rest.allowedMethods (Rest.result (POST : nil))
  # Rest.malformedRequest (\req state -> do
                              body <- allBody req mempty
                              let
                                maybeStreamDetails = hush $ readJSON $ binaryToString body
                              Rest.result (isNothing maybeStreamDetails) req state{streamDetails = maybeStreamDetails})
  # Rest.contentTypesAccepted (\req state ->
                                Rest.result ((tuple2 "application/json" (\req2 state2@{streamDetails: maybeStreamDetails} ->
                                                                          let
                                                                            streamDetails = fromMaybe' (lazyCrashIfMissing "stream_details is nothing") maybeStreamDetails
                                                                          in
                                                                            do
                                                                              -- TODO test load and maybe fail request - test can be shared with local start in IngestInstance, so startAggregator needs to return an Either
                                                                              _ <- IngestAggregatorInstanceSup.startAggregator streamDetails
                                                                              Rest.result true req2 state2
                                                                        )) : nil)
                                req state)
  # Rest.resourceExists (\req state -> Rest.result false req state)

  # Rest.previouslyExisted (Rest.result false)

  # Rest.allowMissingPost (Rest.result true)

  # Rest.contentTypesProvided (\req state -> Rest.result (tuple2 "application/json" (Rest.result ""): nil) req state)

  # Rest.yeeha

type IngestAggregatorsActiveIngestState = { streamAndVariant :: StreamAndVariant
                                          , serverAddress :: Maybe ServerAddress}
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

allBody :: Req -> IOData -> Effect Binary
allBody req acc = do
  readResult <- (readBody req)
  case readResult of
       (FullData body req2) -> pure $ toBinary $ acc <> (fromBinary body)
       (PartialData body req2) -> (allBody req2 $ acc <> (fromBinary body))

binaryToString :: Binary -> String
binaryToString = unsafeCoerce
