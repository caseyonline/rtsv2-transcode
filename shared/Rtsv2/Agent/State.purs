module Shared.Rtsv2.Agent.State
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
       , NodeManager
       , Health
       ) where

import Prelude

import Data.Lens (_Just, firstOf, traversed)
import Data.Maybe (Maybe)
import Data.Traversable (class Traversable)
import Shared.Common (Milliseconds, Url)
import Shared.Rtsv2.JsonLd as JsonLd
import Shared.Rtsv2.Stream (AgentKey, IngestKey, SlotId, SlotRole)
import Shared.Rtsv2.Types (GeoLoc, PoPName, RegionName, Server, ServerAddress)
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

type Health = JsonLd.HealthNode

type NodeManager = JsonLd.NodeManagerStateNode

type Ingest f = JsonLd.IngestStateNode f

type IngestAggregator f = JsonLd.IngestAggregatorStateNode f

type StreamRelay f = JsonLd.StreamRelayStateNode f

type Egest f = JsonLd.EgestStatsNode f

type IntraPoP f = JsonLd.IntraPoPStateNode f

type SlotState f = { aggregators :: f (IngestAggregator f)
                   , ingests :: f (Ingest f)
                   , originRelays :: f (StreamRelay f)
                   , downstreamRelays :: f (StreamRelay f)
                   , egests :: f (Egest f)
                   }

type AgentLocation f = { agentKey :: AgentKey
                       , servers :: f Server
                       }
--foo :: forall f. Functor f => f JsonLd.DownstreamRelayLocationNode -> ServerAddress
foo :: forall f. Traversable f => f JsonLd.DownstreamRelayLocationNode -> Maybe Url
foo nodes =
 firstOf (traversed <<< JsonLd._unwrappedNode <<< JsonLd._id <<< _Just) nodes

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
