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
import Foreign (Foreign)
import Logger (spy)
import Rtsv2.Agents.IngestAggregatorInstance as IngestAggregatorInstance
import Rtsv2.Agents.IngestAggregatorInstanceSup as IngestAggregatorInstanceSup
import Rtsv2.Agents.SlotTypes (SlotConfiguration)
import Rtsv2.Agents.StreamRelayTypes (RegisterRelayPayload)
import Rtsv2.Web.Bindings as Bindings
import Shared.LlnwApiTypes (StreamDetails)
import Shared.Stream (AggregatorKey(..), IngestKey(..), SlotId, SlotRole, ProfileName)
import Shared.Types (ServerAddress)
import Shared.Types.Agent.State as PublicState
import Shared.Utils (lazyCrashIfMissing)
import Simple.JSON (readJSON)
import Stetson (HttpMethod(..), StetsonHandler)
import Stetson.Rest as Rest
import StetsonHelper (GenericStatusState, GenericStetsonHandler, allBody, binaryToString, genericGet, genericPost)

ingestAggregator :: SlotId -> SlotRole -> StetsonHandler (GenericStatusState (PublicState.IngestAggregator List))
ingestAggregator slotId role = genericGet $ IngestAggregatorInstance.getState $ AggregatorKey slotId role

slotConfiguration :: SlotId -> SlotRole -> StetsonHandler (GenericStatusState (Maybe SlotConfiguration))
slotConfiguration slotId role =
  genericGet slotConfigurationBySlotIdAndRole

  where
    slotConfigurationBySlotIdAndRole =
      do
        result <- IngestAggregatorInstance.slotConfiguration (AggregatorKey slotId role)
        let _ = spy "Ingest Aggregator Slot Config" result
        pure result

ingestAggregators :: GenericStetsonHandler StreamDetails
ingestAggregators = genericPost IngestAggregatorInstanceSup.startAggregator


type IngestAggregatorsActiveIngestState = { ingestKey :: IngestKey
                                          , aggregatorKey :: AggregatorKey
                                          , serverAddress :: Maybe ServerAddress
                                          }
ingestAggregatorsActiveIngest :: SlotId -> SlotRole -> ProfileName -> StetsonHandler IngestAggregatorsActiveIngestState
ingestAggregatorsActiveIngest slotId streamRole profileName =
  Rest.handler (\req ->
                 Rest.initResult req { ingestKey: IngestKey slotId streamRole profileName
                                     , aggregatorKey: AggregatorKey slotId streamRole
                                     , serverAddress: Nothing})
  # Rest.serviceAvailable (\req state -> do
                              isAgentAvailable <- IngestAggregatorInstanceSup.isAvailable
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
                                    maybeServerAddress :: Maybe ServerAddress
                                    maybeServerAddress = hush $ readJSON $ binaryToString body
                                  Rest.result (isNothing maybeServerAddress) req state{serverAddress = maybeServerAddress}
                              _ ->
                                Rest.result false req state

                          )
  # Rest.contentTypesAccepted (\req state ->
                                Rest.result ((tuple2 "application/json" (\req2 state2@{ ingestKey
                                                                                      , serverAddress: maybeServerAddress} ->
                                                                          let
                                                                            serverAddress = fromMaybe' (lazyCrashIfMissing "server_address is nothing") maybeServerAddress
                                                                          in
                                                                            do
                                                                              IngestAggregatorInstance.addRemoteIngest ingestKey serverAddress
                                                                              Rest.result true req2 state2
                                                                        )) : nil)
                                req state
                              )


  # Rest.previouslyExisted (Rest.result false)

  # Rest.allowMissingPost (Rest.result false)

  # Rest.deleteResource (\req state@{ingestKey} -> do
                            IngestAggregatorInstance.removeIngest ingestKey
                            Rest.result true req state
                        )

  # Rest.contentTypesProvided (\req state -> Rest.result (tuple2 "application/json" (Rest.result ""): nil) req state)

  # Rest.yeeha


registerRelay :: GenericStetsonHandler RegisterRelayPayload
registerRelay = genericPost IngestAggregatorInstance.registerRelay
