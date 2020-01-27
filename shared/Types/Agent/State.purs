module Shared.Types.Agent.State
       ( Egest
       , IngestAggregator
       , StreamRelay
       ) where

import Shared.LlnwApiTypes (StreamDetails)
import Shared.Stream (StreamVariant)
import Shared.Types (ServerAddress)


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
