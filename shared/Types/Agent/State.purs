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
       , AggregatorLocation
       , StreamRelay
       , TimedPoPStep
       , TimedPoPRoute
       , TimedPoPRoutes

       , ActiveIngestContextFields
       , DownstreamRelayContextFields
       , AggregatorLocationContextFields
       , RelayLocationContextFields
       , EgestLocationContextFields

       ) where

import Prelude

import Data.Maybe (Maybe)
import Shared.Common (Milliseconds)
import Shared.JsonLd as JsonLd
import Shared.LlnwApiTypes (StreamDetails)
import Shared.Stream (AgentKey, IngestKey, ProfileName, SlotId, SlotRole)
import Shared.Types (DeliverTo, GeoLoc, PoPName, RegionName, RelayServer, Server, ServerAddress, ServerRec)
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

type ActiveIngestContextFields = ( profileName :: JsonLd.ContextValue
                                 , serverAddress :: JsonLd.ContextValue
                                 )

type DownstreamRelayContextFields = ( port :: JsonLd.ContextValue
                                    , server :: JsonLd.ContextValue)

type IngestAggregator f
   = { role :: SlotRole
     , streamDetails :: StreamDetails
     , activeProfiles :: f (JsonLd.Node { profileName :: ProfileName
                                        , serverAddress :: ServerAddress
                                        } ActiveIngestContextFields)
     , downstreamRelays :: f (JsonLd.Node (DeliverTo RelayServer) DownstreamRelayContextFields)
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
  = { aggregatorLocations :: f { slotId :: SlotId
                               , role :: SlotRole
                               , servers :: f (JsonLd.Node ServerRec AggregatorLocationContextFields)
                               }
    , relayLocations      :: f { slotId :: SlotId
                               , role :: SlotRole
                               , servers :: f (JsonLd.Node ServerRec RelayLocationContextFields)
                               }
    , egestLocations      :: f { slotId :: SlotId
                               , servers :: f (JsonLd.Node ServerRec EgestLocationContextFields)
                               }
    , currentTransPoPLeader :: Maybe Server
    }

type AggregatorLocationContextFields = ( )

type RelayLocationContextFields = ( )

type EgestLocationContextFields = ( )

type AggregatorLocation f = f { slotId :: SlotId
                              , role :: SlotRole
                              , servers :: f (JsonLd.Node ServerRec AggregatorLocationContextFields)
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
