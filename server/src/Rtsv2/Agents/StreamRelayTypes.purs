module Rtsv2.Agents.StreamRelayTypes
  ( CreateRelayPayload
  , CreateProxyPayload
  , RelayToRelayClientWsMessage(..)
  , EgestClientWsMessage(..)
  , AggregatorToIngestWsMessage(..)
  , DownstreamWsMessage(..)
  , WebSocketHandlerMessage(..)
  ) where

import Prelude

import Foreign (F, Foreign, ForeignError(..), fail)
import Partial.Unsafe (unsafeCrashWith)
import Rtsv2.Agents.SlotTypes (SlotConfiguration)
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

data RelayToRelayClientWsMessage = RelayToRelay Unit

data EgestClientWsMessage = EdgeToRelay Unit

data DownstreamWsMessage = SlotConfig SlotConfiguration

data WebSocketHandlerMessage a = WsStop
                               | WsSend a

------------------------------------------------------------------------------
-- IngestToAggregatorClientWsMessage
instance readForeignAggregatorToIngestWsMessage :: ReadForeign AggregatorToIngestWsMessage where
  readImpl o = decodeValue =<< decodeTag o
    where
      decodeTag :: Foreign -> F {tag :: String, value :: Foreign}
      decodeTag a = readImpl a

      decodeValue :: {tag :: String, value :: Foreign} -> F AggregatorToIngestWsMessage
      -- decodeValue {tag: "register",
      --              value: val} = RelayRegister <$> readImpl val
      decodeValue {tag: "ingestStop"} = pure IngestStop
      decodeValue {tag} = fail $ ForeignError ("unknown tag: " <> tag)

instance writeForeignAggregatorToIngestWsMessage :: WriteForeign AggregatorToIngestWsMessage where
  writeImpl IngestStop = writeImpl { tag: "ingestStop"
                                   , value: 0 }

------------------------------------------------------------------------------
-- RelayToRelayClientWsMessage
instance readForeignRelayToRelayClientWsMessage :: ReadForeign RelayToRelayClientWsMessage where
  readImpl o = fail $ ForeignError ("no client message")

------------------------------------------------------------------------------
-- EgestClientWsMessage
instance readForeignEgestClientWsMessage :: ReadForeign EgestClientWsMessage where
  readImpl o = fail $ ForeignError ("no client message")

------------------------------------------------------------------------------
-- DownstreamWsMessage
instance readForeignDownstreamWsMessage :: ReadForeign DownstreamWsMessage where
  readImpl o = decodeValue =<< decodeTag o
    where
      decodeTag :: Foreign -> F {tag :: String, value :: Foreign}
      decodeTag a = readImpl a

      decodeValue :: {tag :: String, value :: Foreign} -> F DownstreamWsMessage
      decodeValue {tag: "slotConfig",
                   value: val} = SlotConfig <$> readImpl val
      decodeValue {tag} = fail $ ForeignError ("unknown tag: " <> tag)

instance writeForeignDownstreamWsMessage :: WriteForeign DownstreamWsMessage where
  writeImpl (SlotConfig c) = writeImpl { tag: "slotConfig"
                                       , value: writeImpl c}
