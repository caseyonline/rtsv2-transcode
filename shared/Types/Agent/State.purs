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
import Shared.Types (GeoLoc, Milliseconds, PoPName, RegionName, RtmpClientMetadata, Server, ServerAddress)
import Shared.Types.Workflow.Metrics.FrameFlow as FrameFlow
import Shared.Types.Workflow.Metrics.RtmpPushIngest as RtmpIngest
import Shared.Types.Workflow.Metrics.StreamBitrateMonitor as StreamBitrateMonitor

type TimedPoPRoutes f
  = { from :: PoPName
    , to :: PoPName
    , routes :: f (TimedPoPRoute f)
    }

type TimedPoPRoute f
  = f TimedPoPStep

type TimedPoPStep
  = { from :: PoPName
    , to :: PoPName
    , rtt :: Int
    }

type Ingest
  = { rtmpClientMetadata :: Maybe RtmpClientMetadata
    }

type IngestAggregator f
   = { streamDetails :: StreamDetails
     , activeStreamVariants :: f { streamVariant :: StreamVariant
                                 , serverAddress :: ServerAddress
                                 }
     }

type StreamRelay f
  = { egestsServed :: f ServerAddress
    , relaysServed :: f ServerAddress
    }

type Egest
  = { clientCount :: Int
    }


type IntraPoP f
  = { aggregatorLocations :: f { streamId :: StreamId
                               , servers  :: f Server
                               }
    , relayLocations      :: f { streamId :: StreamId
                               , servers  :: f Server
                               }
    , egestLocations      :: f { streamId :: StreamId
                               , servers  :: f Server
                          }
    , currentTransPoPLeader :: Maybe Server
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

type IngestStats f = f { timestamp :: Milliseconds
                       , streamAndVariant :: StreamAndVariant
                       , streamBitrateMetrics :: StreamBitrateMonitor.Metrics f
                       , frameFlowMeterMetrics :: FrameFlow.Metrics f
                       , rtmpIngestMetrics :: RtmpIngest.Metrics
                       }
