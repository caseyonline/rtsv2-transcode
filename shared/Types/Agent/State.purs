module Shared.Types.Agent.State
       ( Egest
       , Ingest
       , IngestAggregator
       , StreamRelay
       , TimedPoPStep
       , TimedPoPRoute
       , TimedPoPRoutes
       ) where

import Data.Maybe (Maybe)
import Shared.LlnwApiTypes (StreamDetails)
import Shared.Stream (StreamVariant)
import Shared.Types (PoPName, RtmpClientMetadata, ServerAddress)

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

type Ingest
  = { rtmpClientMetadata :: Maybe RtmpClientMetadata
    }

type IngestAggregator
   = { streamDetails :: StreamDetails
     , activeStreamVariants :: Array { streamVariant :: StreamVariant
                                     , serverAddress :: ServerAddress
                                     }
     }

type StreamRelay
  = { egestsServed :: Array ServerAddress
    , relaysServed :: Array ServerAddress
    }

type Egest
  = { clientCount :: Int
    }
