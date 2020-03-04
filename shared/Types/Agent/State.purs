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
       ) where


import Data.Maybe (Maybe)

import Shared.LinkedResource (LinkedResource)
import Shared.LlnwApiTypes (StreamDetails)
import Shared.Stream (AgentKey, IngestKey, ProfileName, SlotId, SlotRole)
import Shared.Types (DeliverTo, GeoLoc, Milliseconds, PoPName, RegionName, RelayServer, Server, ServerAddress)
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

-- test =
--   let
--     x = {server : (wrap { address: wrap "serverAddress"
--                         , pop: wrap "pop"
--                         , region: wrap "region"})
--         , port: 3000}

--     y = wrap { resource: x
--              , id : wrap "url here"
--              }

--     z :: IngestAggregator List
--     z = { role : Primary
--         , streamDetails : { role: Primary
--                           , slot: { id: wrap 1
--                                   , name: "slot"
--                                   , subscribeValidation: true
--                                   , outputFormats: []
--                                   , profiles: []
--                                   }
--                           , push: []}
--         , activeProfiles : nil
--         , downstreamRelays : (y : y : nil)
--         }
--   in
--     writeJSON z

-- class AddPrefix a where
--   prefix :: SProxy

-- instance linkedResource :: AddPrefix LinkedResource a where
--   prefix = SProxy "@"

-- class AdderFoo a b where
--   doAdd :: a -> SProxy b -> Foreign

-- instance recordAdderFoo :: ( RL.RowToList row rl
--                            , AddFields rl pre row () to
--                            , IsSymbol pre
--                            ) => AdderFoo (Record row) pre where
--   doAdd rec pre = unsafeToForeign $ foobar
--     where
--       foobar = Builder.build steps {}
--       rlp = RLProxy :: RLProxy rl
--       steps = getFields rlp rec pre

-- class AddFields (rl :: RL.RowList) pre row (from :: # Type) (to :: # Type)
--   | rl -> pre row from to where
--   getFields :: forall g. g rl -> Record row -> SProxy pre -> Builder (Record from) (Record to)

-- instance consAddFields ::
--   ( IsSymbol name
--   , IsSymbol pre
--   , Append pre name newName
--   , IsSymbol newName
--   , AddFields tail pre row from from'
--   , Row.Cons name ty whatever row
--   , Row.Lacks newName from'
--   , Row.Cons newName ty from' to
--   ) => AddFields (RL.Cons name ty tail) pre row from to where
--   getFields _ rec pre = result
--     where
--       namep = SProxy :: SProxy name
--       newNamep = SProxy :: SProxy newName
--       value = R.get namep rec
--       tailp = RLProxy :: RLProxy tail
--       rest = getFields tailp rec pre
--       result = Builder.insert newNamep value <<< rest

-- instance nilAddFields ::
--   AddFields RL.Nil row pre () () where
--   getFields _ _ _ = identity

-- callAddFields :: forall r1 rl1 r2 sym. RL.RowToList r1 rl1 => AddFields rl1 sym r1 () r2 => Record r1 -> SProxy sym -> Record r2
-- callAddFields rec prefix = Builder.build steps {}
--   where
--     steps = getFields rlp rec prefix
--     rlp = RLProxy :: RLProxy rl1

-- test2 =
--   let
--     x :: DeliverTo RelayServer
--     x = {server : (wrap { address: wrap "serverAddress"
--                         , pop: wrap "pop"
--                         , region: wrap "region"})
--         , port: 3000}

--     y = createLinkedResource { resource: x
--                              , id : wrap "url here"
--                              }

--     streamDetails = { role: Primary
--                     , slot: { id: wrap 1
--                             , name: "slot"
--                             , subscribeValidation: true
--                             , outputFormats: []
--                             , profiles: []
--                             }
--                     , push: []}

--     z :: IngestAggregator List
--     z = { role : Primary
--         , streamDetails
--         , activeProfiles : nil
--         , downstreamRelays : singleton y
--         }
--   in
--   callAddFields streamDetails (SProxy :: SProxy "@")


type IngestAggregator f
   = { role :: SlotRole
     , streamDetails :: StreamDetails
     , activeProfiles :: f { profileName :: ProfileName
                           , serverAddress :: ServerAddress
                           }
     , downstreamRelays :: f (LinkedResource (DeliverTo RelayServer))
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
  = { aggregatorLocations :: AggregatorLocation f
    , relayLocations      :: f { slotId :: SlotId
                               , role :: SlotRole
                               , servers :: f Server
                               }
    , egestLocations      :: f { slotId :: SlotId
                               , role :: SlotRole
                               , servers :: f Server
                               }
    , currentTransPoPLeader :: Maybe Server
    }

type AggregatorLocation f = f { slotId :: SlotId
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
