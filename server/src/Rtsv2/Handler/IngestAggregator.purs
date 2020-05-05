module Rtsv2.Handler.IngestAggregator
       ( ingestAggregator
       , ingestAggregators
       , registeredIngestWs
       , registeredRelayWs
       , backupWs
       )
       where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe')
import Effect (Effect)
import Erl.Data.List (List)
import Erl.Process (Process(..))
import Erl.Utils as Erl
import Rtsv2.Agents.IngestAggregatorInstance as IngestAggregatorInstance
import Rtsv2.Agents.IngestAggregatorSup as IngestAggregatorSup
import Rtsv2.Agents.StreamRelayTypes (AggregatorBackupToPrimaryWsMessage, AggregatorPrimaryToBackupWsMessage, AggregatorToIngestWsMessage(..), DownstreamWsMessage(..), IngestToAggregatorWsMessage(..), RelayUpstreamWsMessage(..), WebSocketHandlerMessage(..))
import Rtsv2.Config (LoadConfig)
import Rtsv2.Handler.Helper (WebSocketHandlerResult(..), webSocketHandler)
import Rtsv2.Names as Names
import Rtsv2.PoPDefinition as PoPDefinition
import Shared.Rtsv2.Agent.State as PublicState
import Shared.Rtsv2.LlnwApiTypes (StreamDetails)
import Shared.Rtsv2.Stream (AggregatorKey(..), IngestKey(..), ProfileName, SlotId, SlotRole)
import Shared.Rtsv2.Types (RelayServer(..), Server(..), ServerAddress)
import Shared.Utils (lazyCrashIfMissing)
import Stetson (InnerStetsonHandler)
import StetsonHelper (GetHandler, PostHandler, jsonResponse, processPostPayload)

ingestAggregator :: SlotId -> SlotRole -> GetHandler (PublicState.IngestAggregator List)
ingestAggregator slotId role = jsonResponse $ Just <$> (IngestAggregatorInstance.getState $ AggregatorKey slotId role)

ingestAggregators :: LoadConfig -> PostHandler StreamDetails
ingestAggregators loadConfig = processPostPayload (IngestAggregatorSup.startLocalAggregator loadConfig)

type WsIngestState =
  { ingestKey :: IngestKey
  , aggregatorKey :: AggregatorKey
  }

registeredIngestWs :: SlotId -> SlotRole -> ProfileName -> ServerAddress -> InnerStetsonHandler (WebSocketHandlerMessage AggregatorToIngestWsMessage) WsIngestState
registeredIngestWs slotId slotRole profileName ingestAddress =
  webSocketHandler init wsInit handle info
  where
    init req =
      pure { ingestKey: IngestKey slotId slotRole profileName
           , aggregatorKey: AggregatorKey slotId slotRole
           }
    wsInit state@{aggregatorKey} = do
      self <- Process <$> Erl.self :: Effect (Process (WebSocketHandlerMessage AggregatorToIngestWsMessage))
      result <- IngestAggregatorInstance.registerIngest slotId slotRole profileName ingestAddress self
      case result of
        true -> do
          Erl.monitor (Names.ingestAggregatorInstanceStateName aggregatorKey)
          pure $ WebSocketNoReply state
        false ->
          pure $ WebSocketReply IngestStop state

    handle :: WsIngestState -> IngestToAggregatorWsMessage -> Effect (WebSocketHandlerResult AggregatorToIngestWsMessage WsIngestState)
    handle state@{aggregatorKey} (IngestToAggregatorDataObjectMessage msg) = do
      IngestAggregatorInstance.dataObjectSendMessage aggregatorKey msg
      pure $ WebSocketNoReply state
    handle state@{aggregatorKey} (IngestToAggregatorDataObjectUpdateMessage msg) = do
      IngestAggregatorInstance.dataObjectUpdate aggregatorKey msg
      pure $ WebSocketNoReply state

    info state WsStop =
      pure $ WebSocketStop state
    info state (WsSend msg) =
      pure $ WebSocketReply msg state

type WsRelayState =
  { relayServer :: RelayServer
  , aggregatorKey :: AggregatorKey
  }

registeredRelayWs :: SlotId -> SlotRole -> ServerAddress -> Int -> InnerStetsonHandler (WebSocketHandlerMessage DownstreamWsMessage)  WsRelayState
registeredRelayWs slotId slotRole relayAddress relayPort =
  webSocketHandler init wsInit handle info
  where
    init req = do
      mServer <- PoPDefinition.whereIsServer relayAddress
      let
        Server server = fromMaybe' (lazyCrashIfMissing "unknown server") mServer
      pure { relayServer: Relay server
           , aggregatorKey: AggregatorKey slotId slotRole
           }

    wsInit state@{relayServer, aggregatorKey} = do
      self <- Process <$> Erl.self :: Effect (Process (WebSocketHandlerMessage DownstreamWsMessage))
      Erl.monitor (Names.ingestAggregatorInstanceStateName aggregatorKey)
      slotConfiguration <- IngestAggregatorInstance.registerRelay slotId slotRole {server: relayServer, port: relayPort} self
      pure $ WebSocketReply (SlotConfig slotConfiguration) state

    handle :: WsRelayState -> RelayUpstreamWsMessage -> Effect (WebSocketHandlerResult DownstreamWsMessage WsRelayState)
    handle state@{aggregatorKey} (RelayUpstreamDataObjectMessage msg) = do
      IngestAggregatorInstance.dataObjectSendMessage aggregatorKey msg
      pure $ WebSocketNoReply state
    handle state@{aggregatorKey} (RelayUpstreamDataObjectUpdateMessage msg) = do
      IngestAggregatorInstance.dataObjectUpdate aggregatorKey msg
      pure $ WebSocketNoReply state

    info state WsStop =
      pure $ WebSocketStop state
    info state (WsSend msg) =
      pure $ WebSocketReply msg state

type WsBackupState =
  { aggregatorKey :: AggregatorKey
  }

backupWs :: SlotId -> SlotRole -> InnerStetsonHandler (WebSocketHandlerMessage AggregatorBackupToPrimaryWsMessage) WsBackupState
backupWs slotId slotRole =
  webSocketHandler init wsInit handle info
  where
    init req = do
      pure { aggregatorKey: AggregatorKey slotId slotRole
           }

    wsInit state@{aggregatorKey} = do
      self <- Process <$> Erl.self :: Effect (Process (WebSocketHandlerMessage AggregatorBackupToPrimaryWsMessage))
      accepted <- IngestAggregatorInstance.registerPrimary slotId slotRole self
      if accepted then do
        Erl.monitor (Names.ingestAggregatorInstanceStateName aggregatorKey)
        pure $ (WebSocketNoReply state)
      else
        pure $ (WebSocketStop state)

    handle :: WsBackupState -> AggregatorPrimaryToBackupWsMessage -> Effect (WebSocketHandlerResult AggregatorBackupToPrimaryWsMessage WsBackupState)
    handle state@{aggregatorKey} msg = do
      IngestAggregatorInstance.processMessageFromPrimary aggregatorKey msg
      pure $ WebSocketNoReply state

    info state WsStop =
      pure $ WebSocketStop state
    info state (WsSend msg) =
      pure $ WebSocketReply msg state
