module Rtsv2.Agents.StreamRelay.Types
  ( CreateRelayPayload
  , CreateProxyPayload
  , RegisterRelayPayload
  , RegisterEgestPayload
  , SourceRoute
  ) where

import Erl.Data.List (List)
import Shared.Stream (StreamId)
import Shared.Types (EgestServer, PoPName, RelayServer)

type CreateRelayPayload
  = { streamId :: StreamId
    , aggregatorPoP :: PoPName
    }

type CreateProxyPayload
  = { streamId :: StreamId
    , proxyFor :: PoPName
    , aggregatorPoP :: PoPName
    }

type RegisterRelayPayload
  = { streamId :: StreamId
    , deliverTo :: RelayServer
    , sourceRoute :: SourceRoute
    }

type RegisterEgestPayload
  = { streamId :: StreamId
    , deliverTo :: EgestServer
    }

type SourceRoute = List PoPName
