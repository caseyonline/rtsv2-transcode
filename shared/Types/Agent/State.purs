module Shared.Types.Agent.State
       ( Egest
       , IngestAggregator
       , IntraPoP
       , StreamRelay
       , TimedPoPStep
       , TimedPoPRoute
       , TimedPoPRoutes

       ) where

import Data.Tuple (Tuple(..))
import Shared.LlnwApiTypes (StreamDetails)
import Shared.Stream (StreamId(..), StreamVariant)
import Shared.Types (PoPName, Server(..), ServerAddress, ServerRec)

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
    , relaysServed :: Array ServerAddress
    }

type Egest
  = { clientCount :: Int
    }


type IntraPoP
  = { aggregatorLocations :: Array { streamId :: StreamId
                                   , server :: Server
                                   }
    }
