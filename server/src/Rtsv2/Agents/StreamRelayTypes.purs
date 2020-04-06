module Rtsv2.Agents.StreamRelayTypes
  ( CreateRelayPayload
  , CreateProxyPayload
  , RelayUpstreamWsMessage(..)
  , EgestUpstreamWsMessage(..)
  , AggregatorToIngestWsMessage(..)
  , DownstreamWsMessage(..)
  , WebSocketHandlerMessage(..)
  ) where

import Prelude

import Data.Generic.Rep (class Generic)
import Kishimen (genericSumToVariant, variantToGenericSum)
import Rtsv2.Agents.SlotTypes (SlotConfiguration)
import Rtsv2.DataObject as DO
import Shared.Stream (SlotId, SlotRole)
import Shared.Types (PoPName, Server)
import Simple.JSON (class ReadForeign, class WriteForeign, readImpl, writeImpl)

type CreateRelayPayload
  = { slotId :: SlotId
    , slotRole :: SlotRole
    , aggregator :: Server
    }

type CreateProxyPayload
  = { slotId :: SlotId
    , slotRole :: SlotRole
    , proxyFor :: PoPName
    , aggregator:: Server
    }

data AggregatorToIngestWsMessage = IngestStop
                                 | AggregatorUpstreamDataObjectMessage String

data RelayUpstreamWsMessage = RelayUpstreamDataObjectMessage DO.Message
                            | RelayUpstreamDataObjectUpdateMessage DO.ObjectUpdateMessage

data EgestUpstreamWsMessage = EdgeToRelayDataObjectMessage DO.Message
                            | EdgeToRelayDataObjectUpdateMessage DO.ObjectUpdateMessage

data DownstreamWsMessage = SlotConfig SlotConfiguration
                         | OnFI {timestamp:: Int, pts:: Int}
                         | DataObjectMessage DO.Message
                         | DataObjectUpdateResponse DO.ObjectUpdateResponseMessage
                         | DataObject DO.ObjectBroadcastMessage

data WebSocketHandlerMessage a = WsStop
                               | WsSend a

------------------------------------------------------------------------------
-- IngestToAggregatorClientWsMessage
derive instance genericAggregatorToIngestWsMessage :: Generic AggregatorToIngestWsMessage _

instance readForeignAggregatorToIngestWsMessage :: ReadForeign AggregatorToIngestWsMessage where
  readImpl o = variantToGenericSum <$> readImpl o

instance writeForeignAggregatorToIngestWsMessage :: WriteForeign AggregatorToIngestWsMessage where
  writeImpl msg = writeImpl (genericSumToVariant msg)

------------------------------------------------------------------------------
-- RelayUpstreamWsMessage
derive instance genericRelayUpstreamWsMessage :: Generic RelayUpstreamWsMessage _

instance readForeignRelayUpstreamWsMessage :: ReadForeign RelayUpstreamWsMessage where
  readImpl o = variantToGenericSum <$> readImpl o

instance writeForeignRelayUpstreamWsMessage :: WriteForeign RelayUpstreamWsMessage where
  writeImpl msg = writeImpl (genericSumToVariant msg)

------------------------------------------------------------------------------
-- EgestUpstreamWsMessage
derive instance genericEgestUpstreamWsMessage :: Generic EgestUpstreamWsMessage _

instance readForeignEgestUpstreamWsMessage :: ReadForeign EgestUpstreamWsMessage where
  readImpl o = variantToGenericSum <$> readImpl o

instance writeForeignEgestUpstreamWsMessage :: WriteForeign EgestUpstreamWsMessage where
  writeImpl msg = writeImpl (genericSumToVariant msg)

------------------------------------------------------------------------------
-- DownstreamWsMessage
derive instance genericDownstreamWsMessage :: Generic DownstreamWsMessage _

instance readForeignDownstreamWsMessage :: ReadForeign DownstreamWsMessage where
  readImpl o = variantToGenericSum <$> readImpl o

instance writeForeignDownstreamWsMessage :: WriteForeign DownstreamWsMessage where
  writeImpl msg = writeImpl (genericSumToVariant msg)
