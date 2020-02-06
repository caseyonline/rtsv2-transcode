module Shared.Types.Agent.State
       ( Egest
       , Ingest
       , IngestAggregator
       , IngestStats
       , IntraPoP
       , PoPDefinition
       , Region
       , PoP
       , StreamRelay
       , TimedPoPStep
       , TimedPoPRoute
       , TimedPoPRoutes
       ) where


import Data.Maybe (Maybe)
import Shared.LlnwApiTypes (StreamDetails)
import Shared.Stream (StreamAndVariant, StreamId, StreamVariant)
import Shared.Types (Container, GeoLoc, PoPName, RegionName, RtmpClientMetadata, Server, ServerAddress)
import Shared.Types.Workflow.Metrics.FrameFlow as FrameFlow
import Shared.Types.Workflow.Metrics.RtmpPushIngest as RtmpIngest
import Shared.Types.Workflow.Metrics.StreamBitrateMonitor as StreamBitrateMonitor

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
    , relayLocations :: Array { streamId :: StreamId
                              , servers :: Array Server
                              }
    }

type Region f = { name :: RegionName
                , pops :: f (PoP f)
                }

type PoP f = { name :: PoPName
             , geoLoc :: f GeoLoc
             , servers :: f ServerAddress
             , neighbours :: f PoPName
             }

type PoPDefinition f
  = { regions :: f (Region f)
    , neighbourMap :: f { popName :: PoPName,
                          neighbours :: f PoPName
                        }
    }

type IngestStats f = f { streamAndVariant :: StreamAndVariant
                       , streamBitrateMetrics :: StreamBitrateMonitor.Metrics f
                       , frameFlowMeterMetrics :: FrameFlow.Metrics f
                       , rtmpIngestMetrics :: RtmpIngest.Metrics
                       }
