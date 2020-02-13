module Rtsv2.Agents.StreamRelay.Types
  ( CreateRelayPayload
  , CreateProxyPayload
  , RegisterRelayPayload
  , RegisterEgestPayload
  , SourceRoute
  ) where

import Erl.Data.List (List)
import Shared.Stream (StreamId, StreamRole)
import Shared.Types (EgestServer, PoPName, RelayServer)

type CreateRelayPayload
  = { streamId :: StreamId
    , streamRole :: StreamRole
    , aggregatorPoP :: PoPName
    }

type CreateProxyPayload
  = { streamId :: StreamId
    , streamRole :: StreamRole
    , proxyFor :: PoPName
    , aggregatorPoP :: PoPName
    }

type RegisterRelayPayload
  = { streamId :: StreamId
    , streamRole :: StreamRole
    , deliverTo :: RelayServer
    , sourceRoute :: SourceRoute
    }

type RegisterEgestPayload
  = { streamId :: StreamId
    , streamRole :: StreamRole
    , deliverTo :: EgestServer
    }

type SourceRoute = List PoPName
