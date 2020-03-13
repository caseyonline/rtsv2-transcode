module Shared.Types.Agent.State
       ( Egest
       , Ingest
       , IngestAggregator
       , IngestStats
       , IntraPoP
       , PoPDefinition
       , SlotState
       , Region
       , PoP
       , AgentLocation
       , AggregatorLocation
       , StreamRelay
       , TimedPoPNeighbour
       , TimedPoPStep
       , TimedPoPRoute
       , TimedPoPRoutes

       ) where

import Prelude

import Shared.Common (Milliseconds)
import Shared.Rtsv2.JsonLd as JsonLd
import Shared.Stream (AgentKey, IngestKey, SlotId, SlotRole)
import Shared.Types (GeoLoc, PoPName, RegionName, Server, ServerAddress)
import Shared.Types.Workflow.Metrics.FrameFlow as FrameFlow
import Shared.Types.Workflow.Metrics.RtmpPushIngest as RtmpIngest
import Shared.Types.Workflow.Metrics.StreamBitrateMonitor as StreamBitrateMonitor

type TimedPoPNeighbour f = JsonLd.TimedRouteNeighbourNode f

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

type Ingest f = JsonLd.IngestStateNode f

type IngestAggregator f = JsonLd.IngestAggregatorStateNode f

type StreamRelay f = JsonLd.StreamRelayStateNode f

type Egest = JsonLd.EgestStatsNode

type IntraPoP f = JsonLd.IntraPoPStateNode f

type SlotState f = { aggregators :: f (IngestAggregator f)
                   , ingests :: f (Ingest f)
                   , originRelays :: f (StreamRelay f)
                   , downstreamRelays :: f (StreamRelay f)
                   , egests :: f Egest
                   }

type AgentLocation f = { agentKey :: AgentKey
                       , servers :: f Server
                       }

type AggregatorLocation f = { slotId :: SlotId
                            , role :: SlotRole
                            , servers :: f Server
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

type IngestStats f = { timestamp :: Milliseconds
                     , ingestKey :: IngestKey
                     , streamBitrateMetrics :: StreamBitrateMonitor.Metrics f
                     , frameFlowMeterMetrics :: FrameFlow.Metrics f
                     , rtmpIngestMetrics :: RtmpIngest.Metrics
                     }
