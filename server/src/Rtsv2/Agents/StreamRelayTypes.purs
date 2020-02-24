module Rtsv2.Agents.StreamRelayTypes
  ( CreateRelayPayload
  , CreateProxyPayload
  , RegisterRelayPayload
  , RegisterEgestPayload
  , SourceRoute
  , DeliverTo
  ) where

import Erl.Data.List (List)
import Shared.Stream (SlotId, SlotRole)
import Shared.Types (Server, EgestServer, PoPName, RelayServer)

type CreateRelayPayload
  = { streamId :: SlotId
    , streamRole :: SlotRole
    , aggregator :: Server
    }

type CreateProxyPayload
  = { streamId :: SlotId
    , streamRole :: SlotRole
    , proxyFor :: PoPName
    , aggregator:: Server
    }

type DeliverTo serverType
  = { server :: serverType
    , port :: Int
    }

type RegisterRelayPayload
  = { streamId :: SlotId
    , streamRole :: SlotRole
    , deliverTo :: DeliverTo RelayServer
    , sourceRoute :: SourceRoute
    }

type RegisterEgestPayload
  = { streamId :: SlotId
    , streamRole :: SlotRole
    , deliverTo :: DeliverTo EgestServer
    }

type SourceRoute = List PoPName
