module Rtsv2.Agents.StreamRelayTypes
  ( CreateRelayPayload
  , CreateProxyPayload
  , RegisterRelayPayload
  , RegisterEgestPayload
  , DeRegisterRelayPayload
  , DeRegisterEgestPayload
  , SourceRoute
  ) where

import Erl.Data.List (List)
import Rtsv2.Agents.Locator.Types (LocalOrRemote)
import Shared.Stream (SlotId, SlotRole)
import Shared.Types (DeliverTo, EgestServer, PoPName, RelayServer, Server, ServerAddress(..))

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
    , deliverTo :: DeliverTo RelayServer
    , sourceRoute :: SourceRoute
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

type SourceRoute = List PoPName
