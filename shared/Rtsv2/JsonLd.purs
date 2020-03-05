module Shared.Rtsv2.JsonLd
       (
         activeIngestNode
       , downstreamRelayNode
       , aggregatorLocationNode
       , relayLocationNode
       , egestLocationNode
       ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap, wrap)
import Foreign (Foreign, F)
import Shared.JsonLd as JsonLd
import Shared.Router.Endpoint (Endpoint(..), makeUrl, makeUrlAddr)
import Shared.Stream (ProfileName, SlotId, SlotRole)
import Shared.Types (DeliverTo, RelayServer, Server, ServerAddress, Server)
import Shared.Types.Agent.State (AggregatorLocationContextFields, DownstreamRelayContextFields, EgestLocationContextFields, RelayLocationContextFields, ActiveIngestContextFields)
import Simple.JSON (E, readJSON, writeJSON)

activeIngestNode :: SlotId -> SlotRole -> ProfileName -> ServerAddress -> (JsonLd.Node { profileName :: ProfileName
                                                                                       , serverAddress :: ServerAddress
                                                                                       } ActiveIngestContextFields)
activeIngestNode slotId slotRole profileName serverAddress =
  wrap { resource: { profileName
                   , serverAddress}
       , "@id": Just $ makeUrlAddr serverAddress (IngestInstanceE slotId slotRole profileName)
       , "@nodeType": Just "http://schema.rtsv2.llnw.com/ActiveIngest"
       , "@context": Just $ wrap { "@language": Nothing
                                 , "@base": Nothing
                                 , "@vocab": Nothing
                                 , profileName: JsonLd.Other "http://schema.rtsv2.llwn.com/Media/ProfileNam"
                                 , serverAddress: JsonLd.Other "http://schema.rtsv2.llnw.com/ServerAddress"
                                 }
       }


downstreamRelayNode :: SlotId -> SlotRole -> DeliverTo RelayServer -> JsonLd.Node (DeliverTo RelayServer) DownstreamRelayContextFields
downstreamRelayNode slotId slotRole relay@{server} =
  wrap { resource: relay
       , "@id": Just $ makeUrl server (RelayStatsE slotId slotRole)
       , "@nodeType": Just "http://schema.rtsv2.llnw.com/DownstreamRelay"
       , "@context": Just $ wrap { "@language": Nothing
                                 , "@base": Nothing
                                 , "@vocab": Nothing
                                 , port: JsonLd.Other "http://schema.rtsv2.llwn.com/Network/Port"
                                 , server: JsonLd.Other "http://schema.rtsv2.llnw.com/Server"
                                 }
       }

aggregatorLocationNode :: SlotId -> SlotRole -> Server -> JsonLd.Node Server AggregatorLocationContextFields
aggregatorLocationNode slotId slotRole server =
  wrap { resource: server
       , "@id": Just $ makeUrl server (IngestAggregatorE slotId slotRole)
       , "@nodeType": Just "http://schema.rtsv2.llnw.com/AggregatorLocation"
       , "@context": Just $ wrap { "@language": Nothing
                                 , "@base": Nothing
                                 , "@vocab": Nothing
                                 }
       }

relayLocationNode :: SlotId -> SlotRole -> Server -> JsonLd.Node Server RelayLocationContextFields
relayLocationNode slotId slotRole server =
  wrap { resource: server
       , "@id": Just $ makeUrl server (RelayStatsE slotId slotRole)
       , "@nodeType": Just "http://schema.rtsv2.llnw.com/RelayLocation"
       , "@context": Just $ wrap { "@language": Nothing
                                 , "@base": Nothing
                                 , "@vocab": Nothing
                                 }
       }

egestLocationNode :: SlotId -> Server -> JsonLd.Node Server EgestLocationContextFields
egestLocationNode slotId server =
  wrap { resource: server
       , "@id": Just $ makeUrl server (EgestStatsE slotId)
       , "@nodeType": Just "http://schema.rtsv2.llnw.com/EgestLocation"
       , "@context": Just $ wrap { "@language": Nothing
                                 , "@base": Nothing
                                 , "@vocab": Nothing
                                 }
       }

foo :: forall resourceType otherFields newtypeType. Newtype newtypeType { resource :: resourceType | otherFields } => newtypeType -> resourceType
foo = _.resource <<< unwrap

bar :: JsonLd.Node Server () -> String
bar server =
  writeJSON server

baz :: String -> E (JsonLd.Node Server ())
baz s =
  readJSON s
