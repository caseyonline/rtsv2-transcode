module Rtsv2.Agents.StreamRelayTypes
  ( CreateRelayPayload
  , CreateProxyPayload
  , RelayToRelayClientWsMessage(..)
  , EgestClientWsMessage(..)
  , DownstreamWsMessage(..)
  , WebSocketHandlerMessage(..)
  ) where

import Prelude

import Foreign (F, Foreign, ForeignError(..), fail)
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

data RelayToRelayClientWsMessage = RelayToRelay Unit

data EgestClientWsMessage = EdgeToRelay Unit

data DownstreamWsMessage = SlotConfig SlotConfiguration

data WebSocketHandlerMessage = WsStop
                             | WsSend DownstreamWsMessage

------------------------------------------------------------------------------
-- RelayToRelayClientWsMessage
instance readForeignRelayToRelayClientWsMessage :: ReadForeign RelayToRelayClientWsMessage where
  readImpl o = fail $ ForeignError ("no client message")

-- instance readForeignRelayToRelayClientWsMessage :: ReadForeign RelayToRelayClientWsMessage where
--   readImpl o = decodeValue =<< decodeTag o
--     where
--       decodeTag :: Foreign -> F {tag :: String, value :: Foreign}
--       decodeTag a = readImpl a

--       decodeValue :: {tag :: String, value :: Foreign} -> F RelayToRelayClientWsMessage
--       decodeValue {tag: "register",
--                    value: val} = RelayRegister <$> readImpl val
--       decodeValue {tag} = fail $ ForeignError ("unknown tag: " <> tag)

-- instance writeForeignRelayToRelayClientWsMessage :: WriteForeign RelayToRelayClientWsMessage where
--   writeImpl (RelayRegister r) = writeImpl { tag: "register"
--                                           , value: writeImpl r}

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
