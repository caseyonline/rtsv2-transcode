module Shared.Types.Agent.State
       ( Egest
       , IngestAggregator
       , StreamRelay
       , TimedPoPStep
       , TimedPoPRoute
       , TimedPoPRoutes
       ) where

import Shared.LlnwApiTypes (StreamDetails)
import Shared.Stream (StreamVariant)
import Shared.Types (PoPName, ServerAddress)

type TimedPoPRoutes
  = { from :: PoPName
    , to :: PoPName
    , routes :: Array TimedPoPRoute
    }

type TimedPoPRoute
  = Array TimedPoPStep

type TimedPoPStep
  = { from :: PoPName
    , to :: PoPName
    , rtt :: Int
    }

type IngestAggregator
   = { streamDetails :: StreamDetails
     , activeStreamVariants :: Array { streamVariant :: StreamVariant
                                     , serverAddress :: ServerAddress
                                     }
     }

type StreamRelay
  = { egestsServed :: Array ServerAddress
    }

type Egest
  = { clientCount :: Int
    }
