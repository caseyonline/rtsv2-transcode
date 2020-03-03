module Rtsv2.Agents.StreamRelayTypes
  ( CreateRelayPayload
  , CreateProxyPayload
  , RegisterRelayPayload
  , RegisterEgestPayload
  , SourceRoute
  ) where

import Erl.Data.List (List)
import Shared.Stream (SlotId, SlotRole)
import Shared.Types (Server, DeliverTo, EgestServer, PoPName, RelayServer)

type CreateRelayPayload
  = { slotId :: SlotId
    , streamRole :: SlotRole
    , aggregator :: Server
    }

type CreateProxyPayload
  = { slotId :: SlotId
    , streamRole :: SlotRole
    , proxyFor :: PoPName
    , aggregator:: Server
    }

type RegisterRelayPayload
  = { slotId :: SlotId
    , streamRole :: SlotRole
    , deliverTo :: DeliverTo RelayServer
    , sourceRoute :: SourceRoute
    }

type RegisterEgestPayload
  = { slotId :: SlotId
    , streamRole :: SlotRole
    , deliverTo :: DeliverTo EgestServer
    }

type SourceRoute = List PoPName
