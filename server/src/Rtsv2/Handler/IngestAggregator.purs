module Rtsv2.Handler.IngestAggregator
       ( ingestAggregator
       , ingestAggregators
       , ingestAggregatorsActiveIngest
       , registerRelay
       , slotConfiguration
       )
       where

import Prelude

import Data.Either (hush)
import Data.Maybe (Maybe(..), fromMaybe', isNothing)
import Erl.Cowboy.Req (method)
import Erl.Data.List (List, nil, (:))
import Erl.Data.Tuple (tuple2)
import Logger (spy)
import Rtsv2.Agents.IngestAggregatorInstance (RemoteIngestPayload)
import Rtsv2.Agents.IngestAggregatorInstance as IngestAggregatorInstance
import Rtsv2.Agents.IngestAggregatorSup as IngestAggregatorSup
import Rtsv2.Agents.SlotTypes (SlotConfiguration)
import Rtsv2.Agents.StreamRelayTypes (RegisterRelayPayload)
import Shared.LlnwApiTypes (StreamDetails)
import Shared.Stream (AggregatorKey(..), IngestKey(..), ProfileName, SlotId, SlotRole)
import Shared.Types (ServerAddress)
import Shared.Types.Agent.State as PublicState
import Shared.Utils (lazyCrashIfMissing)
import Simple.JSON (readJSON)
import Stetson (HttpMethod(..), StetsonHandler)
import Stetson.Rest as Rest
import StetsonHelper (GetHandler, PostHandler, allBody, binaryToString, jsonResponse, preHookSpyState, processPostPayload)

ingestAggregator :: SlotId -> SlotRole -> GetHandler (PublicState.IngestAggregator List)
ingestAggregator slotId role = jsonResponse $ Just <$> (IngestAggregatorInstance.getState $ AggregatorKey slotId role)

slotConfiguration :: SlotId -> SlotRole -> GetHandler SlotConfiguration
slotConfiguration slotId role =
  jsonResponse $ IngestAggregatorInstance.slotConfiguration (AggregatorKey slotId role)

ingestAggregators :: PostHandler StreamDetails
ingestAggregators = processPostPayload IngestAggregatorSup.startAggregator


type IngestAggregatorsActiveIngestState = { ingestKey :: IngestKey
                                          , aggregatorKey :: AggregatorKey
                                          , payload :: Maybe RemoteIngestPayload
                                          }
ingestAggregatorsActiveIngest :: SlotId -> SlotRole -> ProfileName -> StetsonHandler IngestAggregatorsActiveIngestState
ingestAggregatorsActiveIngest slotId streamRole profileName =
  Rest.handler (\req ->
                 Rest.initResult req { ingestKey: IngestKey slotId streamRole profileName
                                     , aggregatorKey: AggregatorKey slotId streamRole
                                     , payload: Nothing})
  # Rest.serviceAvailable (\req state -> do
                              isAgentAvailable <- IngestAggregatorSup.isAgentAvailable
                              Rest.result isAgentAvailable req state)
  # Rest.allowedMethods (Rest.result (DELETE : POST : nil))
  # Rest.resourceExists (\req state@{aggregatorKey} -> do
                          isAvailable <- IngestAggregatorInstance.isAvailable aggregatorKey
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
                                    maybePayload :: Maybe RemoteIngestPayload
                                    maybePayload = hush $ readJSON $ binaryToString body
                                  Rest.result (isNothing maybePayload) req state{payload = maybePayload}
                              _ ->
                                Rest.result false req state

                          )
  # Rest.contentTypesAccepted (\req state ->
                                Rest.result ((tuple2 "application/json" (\req2 state2@{ ingestKey
                                                                                      , payload: maybePayload} ->
                                                                          let
                                                                            payload = fromMaybe' (lazyCrashIfMissing "payload is nothing") maybePayload
                                                                          in
                                                                            do
                                                                              result <- IngestAggregatorInstance.addRemoteIngest ingestKey payload
                                                                              Rest.result (spy "remoteAdd said" result) req2 state2
                                                                        )) : nil)
                                req state
                              )


  # Rest.previouslyExisted (Rest.result false)

  # Rest.allowMissingPost (Rest.result false)

  # Rest.deleteResource (\req state@{ingestKey} -> do
                            IngestAggregatorInstance.removeRemoteIngest ingestKey
                            Rest.result true req state
                        )

  # Rest.contentTypesProvided (\req state -> Rest.result (tuple2 "application/json" (Rest.result ""): nil) req state)
  -- # Rest.preHook (preHookSpyState "IngestAggregator:activeIngest")
  # Rest.yeeha


registerRelay :: PostHandler RegisterRelayPayload
registerRelay = processPostPayload IngestAggregatorInstance.registerRelay
