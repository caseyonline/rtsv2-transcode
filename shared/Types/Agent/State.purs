module Shared.Types.Agent.State
       ( Egest
       , Ingest
       , IngestAggregator
       , IngestStats
       , IntraPoP
       , PoPDefinition
       , Region
       , PoP
       , AgentLocation
       , StreamRelay
       , TimedPoPStep
       , TimedPoPRoute
       , TimedPoPRoutes
       ) where


import Data.Maybe (Maybe)
import Shared.LlnwApiTypes (StreamDetails)
import Shared.Stream (AgentKey, StreamAndVariant, StreamId, StreamRole, StreamVariant)
import Shared.Types (GeoLoc, Milliseconds, PoPName, RegionName, Server, ServerAddress, ServerRec)
import Shared.Types.Media.Types.Rtmp (RtmpClientMetadata)
import Shared.Types.Media.Types.SourceDetails (SourceInfo)
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

type Ingest f
  = { ingestStartedTime :: Milliseconds
    , remoteAddress :: String
    , remotePort :: Int
    , rtmpClientMetadata :: Maybe (RtmpClientMetadata f)
    , sourceInfo :: Maybe (SourceInfo f)
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


type AgentLocation f = { agentKey :: AgentKey
                       , servers :: f Server
                       }
type IntraPoP f
  = { aggregatorLocations :: f (AgentLocation f)
    , relayLocations      :: f (AgentLocation f)
    , egestLocations      :: f (AgentLocation f)
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
