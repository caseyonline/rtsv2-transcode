module Shared.Rtsv2.JsonLd
       (
         downstreamRelayNode
       ) where

import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Prelude (($))
import Shared.JsonLd as JsonLd
import Shared.Router.Endpoint (Endpoint(..), makeUrl)
import Shared.Stream (SlotId, SlotRole)
import Shared.Types (DeliverTo, RelayServer)
import Shared.Types.Agent.State (DownstreamRelayContextFields)

downstreamRelayNode :: SlotId -> SlotRole -> DeliverTo RelayServer -> JsonLd.Node (DeliverTo RelayServer) DownstreamRelayContextFields
downstreamRelayNode slotId slotRole relay@{server} =
  wrap { resource: relay
       , "@id": Just $ makeUrl server (RelayStatsE slotId slotRole)
       , "@nodeType": Just "http://schema.rtsv2.llnw.com/Relay"
       , "@context": Just $ wrap { "@language": Nothing
                              , "@base": Nothing
                              , "@vocab": Nothing
                              , port: JsonLd.Other "http://schema.org/Integer"
                              , server: JsonLd.Other "http://schema.rtsv2.llnw.com/Server"
                              }
       }
