module Rtsv2.Handler.IngestAggregator
       ( ingestAggregator
       , ingestAggregators
       , ingestAggregatorsActiveIngest
       -- , registerRelay
       -- , deRegisterRelay
--       , slotConfiguration
       , registeredRelayWs
       )
       where

import Prelude

import Data.Either (hush)
import Data.Maybe (Maybe(..), fromMaybe', isNothing)
import Effect (Effect)
import Erl.Cowboy.Req (method)
import Erl.Data.List (List, nil, (:))
import Erl.Data.Tuple (tuple2)
import Erl.Process (Process(..))
import Erl.Utils as Erl
import Logger (spy)
import Rtsv2.Agents.IngestAggregatorInstance (RemoteIngestPayload)
import Rtsv2.Agents.IngestAggregatorInstance as IngestAggregatorInstance
import Rtsv2.Agents.IngestAggregatorSup as IngestAggregatorSup
import Rtsv2.Agents.StreamRelayTypes (DownstreamWsMessage(..), RelayToRelayClientWsMessage, WebSocketHandlerMessage(..))
import Rtsv2.Handler.Helper (WebSocketHandlerResult(..), webSocketHandler)
import Rtsv2.PoPDefinition as PoPDefinition
import Shared.LlnwApiTypes (StreamDetails)
import Shared.Stream (AggregatorKey(..), IngestKey(..), ProfileName, SlotId, SlotRole)
import Shared.Types (RelayServer(..), ServerAddress, ServerLocation(..))
import Shared.Types.Agent.State as PublicState
import Shared.Utils (lazyCrashIfMissing)
import Simple.JSON (readJSON)
import Stetson (HttpMethod(..), InnerStetsonHandler, StetsonHandler)
import Stetson.Rest as Rest
import StetsonHelper (GetHandler, PostHandler, allBody, binaryToString, jsonResponse, processPostPayload)

ingestAggregator :: SlotId -> SlotRole -> GetHandler (PublicState.IngestAggregator List)
ingestAggregator slotId role = jsonResponse $ Just <$> (IngestAggregatorInstance.getState $ AggregatorKey slotId role)

ingestAggregators :: PostHandler StreamDetails
ingestAggregators = processPostPayload IngestAggregatorSup.startAggregator


type IngestAggregatorsActiveIngestState = { ingestKey :: IngestKey
                                          , aggregatorKey :: AggregatorKey
                                          , payload :: Maybe RemoteIngestPayload
                                          }
ingestAggregatorsActiveIngest :: SlotId -> SlotRole -> ProfileName -> StetsonHandler IngestAggregatorsActiveIngestState
ingestAggregatorsActiveIngest slotId slotRole profileName =
  Rest.handler (\req ->
                 Rest.initResult req { ingestKey: IngestKey slotId slotRole profileName
                                     , aggregatorKey: AggregatorKey slotId slotRole
                                     , payload: Nothing})
  # Rest.serviceAvailable (\req state -> do
                              isAgentAvailable <- IngestAggregatorSup.isAgentAvailable
                              Rest.result isAgentAvailable req state)
  # Rest.allowedMethods (Rest.result (DELETE : POST : nil))
  # Rest.resourceExists (\req state@{aggregatorKey} -> do
                          isAvailable <- IngestAggregatorInstance.isInstanceAvailable aggregatorKey
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
                                                                              Rest.result result req2 state2
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

-- registerRelay :: PostHandler RegisterRelayPayload
type WsRelayState =
  { relayServer :: RelayServer
  , aggregatorKey :: AggregatorKey
  }

registeredRelayWs :: SlotId -> SlotRole -> ServerAddress -> Int -> InnerStetsonHandler WebSocketHandlerMessage WsRelayState
registeredRelayWs slotId slotRole relayAddress relayPort =
  webSocketHandler init wsInit handle info
  where
    init req = do
      mServerLocation <- PoPDefinition.whereIsServer relayAddress
      let
        ServerLocation {pop, region} = fromMaybe' (lazyCrashIfMissing "unknown server") mServerLocation
      pure { relayServer: Relay { address: relayAddress
                                , pop
                                , region
                                }
           , aggregatorKey: AggregatorKey slotId slotRole
           }

    wsInit state@{relayServer} = do
      self <- Process <$> Erl.self :: Effect (Process DownstreamWsMessage)
      slotConfiguration <- IngestAggregatorInstance.registerRelay slotId slotRole {server: relayServer, port: relayPort} self
      pure $ WebSocketReply (SlotConfig slotConfiguration) state

    handle :: WsRelayState -> RelayToRelayClientWsMessage -> Effect (WebSocketHandlerResult DownstreamWsMessage WsRelayState)
    handle state _ = do
      pure $ WebSocketNoReply state

    info state WsStop =
      pure $ WebSocketStop state
    info state (WsSend msg) =
      pure $ WebSocketReply msg state
