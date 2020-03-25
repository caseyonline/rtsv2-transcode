module Rtsv2.Agents.StreamRelayTypes
  ( CreateRelayPayload
  , CreateProxyPayload
  , RegisterRelayPayload
  , RegisterEgestPayload
  , DeRegisterRelayPayload
  , DeRegisterEgestPayload
  , SourceRoute
  , EgestClientWsMessage(..)
  , EgestServerWsMessage
  ) where

import Prelude

import Erl.Data.List (List)
import Foreign (F, Foreign, ForeignError(..), fail)
import Rtsv2.Agents.Locator.Types (LocalOrRemote)
import Shared.Stream (SlotId, SlotRole)
import Shared.Types (DeliverTo, EgestServer, PoPName, RelayServer, Server, ServerAddress(..))
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

type RegisterRelayPayload
  = { slotId :: SlotId
    , slotRole :: SlotRole
    , deliverTo :: DeliverTo RelayServer
    , sourceRoute :: SourceRoute
    }

type DeRegisterRelayPayload
  = { slotId :: SlotId
    , slotRole :: SlotRole
    , relayServerAddress :: ServerAddress
    }

type RegisterEgestPayload
  = { slotId :: SlotId
    , slotRole :: SlotRole
    , deliverTo :: DeliverTo EgestServer
    }

type DeRegisterEgestPayload
  = { slotId :: SlotId
    , slotRole :: SlotRole
    , egestServerAddress :: ServerAddress
    }

data EgestClientWsMessage = Register {ourSlotId :: SlotId, ourSlotRole :: SlotRole }
type EgestServerWsMessage = String

type SourceRoute = List PoPName

instance readForeignEgestClientWsMessage :: ReadForeign EgestClientWsMessage where
  readImpl o = decodeValue =<< decodeTag o
    where
      decodeTag :: Foreign -> F {tag :: String, value :: Foreign}
      decodeTag a = readImpl a

      decodeValue :: {tag :: String, value :: Foreign} -> F EgestClientWsMessage
      decodeValue {tag: "register",
                   value: val} = Register <$> readImpl val
      decodeValue {tag} = fail $ ForeignError ("unknown tag: " <> tag)

instance writeForeignEgestClientWsMessage :: WriteForeign EgestClientWsMessage where
  writeImpl (Register r) = writeImpl { tag: "register"
                                     , value: writeImpl r}
