module Shared.Types.Agent.State
       ( Egest
       , Ingest
       , IngestAggregator
       , IngestStats
       , IntraPoP
       , StreamRelay
       , TimedPoPStep
       , TimedPoPRoute
       , TimedPoPRoutes
       ) where


import Data.Maybe (Maybe)
import Shared.LlnwApiTypes (StreamDetails)
import Shared.Stream (StreamAndVariant(..), StreamId, StreamVariant)
import Shared.Types (Container, PoPName, RtmpClientMetadata, Server, ServerAddress, WorkflowMetric)

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


type IntraPoP
  = { aggregatorLocations :: Array { streamId :: StreamId
                                   , servers :: Array Server
                                   }
    }

type IngestStats a = Container a { streamAndVariant :: StreamAndVariant
                                 , metrics :: Container a { source :: String
                                                          , metrics :: Container a (WorkflowMetric a)
                                                          }
                                 }
