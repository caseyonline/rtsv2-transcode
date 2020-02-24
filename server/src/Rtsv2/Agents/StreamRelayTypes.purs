module Rtsv2.Agents.StreamRelayTypes
  ( CreateRelayPayload
  , CreateProxyPayload
  , RegisterRelayPayload
  , RegisterEgestPayload
  , SourceRoute
  , DeliverTo
  ) where

import Erl.Data.List (List)
import Shared.Stream (StreamId, StreamRole)
import Shared.Types (Server, EgestServer, PoPName, RelayServer)

type CreateRelayPayload
  = { streamId :: StreamId
    , streamRole :: StreamRole
    , aggregator :: Server
    }

type CreateProxyPayload
  = { streamId :: StreamId
    , streamRole :: StreamRole
    , proxyFor :: PoPName
    , aggregator:: Server
    }

type DeliverTo serverType
  = { server :: serverType
    , port :: Int
    }

type RegisterRelayPayload
  = { streamId :: StreamId
    , streamRole :: StreamRole
    , deliverTo :: DeliverTo RelayServer
    , sourceRoute :: SourceRoute
    }

type RegisterEgestPayload
  = { streamId :: StreamId
    , streamRole :: StreamRole
    , deliverTo :: DeliverTo EgestServer
    }

type SourceRoute = List PoPName
