module Shared.Rtsv2.JsonLd
       (
         LocationBySlotIdAndSlotRole
       , LocationBySlotId
       , ServerNode
       , ServerAddressNode
       , DeliverToNode
       , EgestServedLocationNode
       , RelayServedLocationNode
       , ActiveIngestLocationNode
       , ActiveIngestLocation
       , AggregatorLocationNode
       , DownstreamRelayLocationNode
       , RelayLocationNode
       , EgestLocationNode
       , ServerContextFields
       , ServerAddressContextFields
       , ActiveIngestContextFields
       , DeliverToContextFields
       , TransPoPLeaderLocationNode

       , activeIngestLocationNode
       , downstreamRelayLocationNode
       , aggregatorLocationNode
       , relayLocationNode
       , egestLocationNode
       , transPoPLeaderLocationNode
       , egestServedLocationNode
       , relayServedLocationNode
       , module JsonLd
       ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Shared.JsonLd (Context, ContextValue(..), ExpandedTermDefinition, Node, unwrapNode) as JsonLd
import Shared.Router.Endpoint (Endpoint(..), makeUrl, makeUrlAddr)
import Shared.Stream (ProfileName, SlotId, SlotRole)
import Shared.Types (DeliverTo, RelayServer, Server, ServerAddress, extractPoP)


------------------------------------------------------------------------------
-- Server
type ServerContextFields = ( address :: JsonLd.ContextValue
                           , pop :: JsonLd.ContextValue
                           , region :: JsonLd.ContextValue
                           )

type ServerNode = JsonLd.Node Server ServerContextFields

------------------------------------------------------------------------------
-- ServerAddress
type ServerAddressContextFields = ( address :: JsonLd.ContextValue )

type ServerAddressNode = JsonLd.Node { address :: ServerAddress } ServerAddressContextFields

------------------------------------------------------------------------------
-- DeliverTo
type DeliverToContextFields = ( port :: JsonLd.ContextValue
                              , server :: JsonLd.ContextValue
                              )

type DeliverToNode a = JsonLd.Node (DeliverTo a) DeliverToContextFields


------------------------------------------------------------------------------
-- LocationByXXX
type LocationBySlotIdAndSlotRole collectionType = { slotId :: SlotId
                                                  , role :: SlotRole
                                                  , servers :: collectionType ServerNode
                                                  }

type LocationBySlotId collectionType = { slotId :: SlotId
                                       , servers :: collectionType ServerNode
                                       }

------------------------------------------------------------------------------
-- ActiveIngest
type ActiveIngestContextFields = ( profileName :: JsonLd.ContextValue
                                 , serverAddress :: JsonLd.ContextValue
                                 )

type ActiveIngestLocation = { profileName :: ProfileName
                            , serverAddress :: ServerAddress
                            }

type ActiveIngestLocationNode = JsonLd.Node ActiveIngestLocation ActiveIngestContextFields

------------------------------------------------------------------------------
-- TransPoPLeaderLocation
type TransPoPLeaderLocationNode = ServerNode

------------------------------------------------------------------------------
-- EgestServedLocation
type EgestServedLocationNode = ServerAddressNode

------------------------------------------------------------------------------
-- RelayServedLocation
type RelayServedLocationNode = ServerAddressNode

------------------------------------------------------------------------------
-- DownstreamRelay
type DownstreamRelayLocationNode = DeliverToNode RelayServer

------------------------------------------------------------------------------
-- AggregatorLocation
type AggregatorLocationNode f =  LocationBySlotIdAndSlotRole f

------------------------------------------------------------------------------
-- RelayLocation
type RelayLocationNode f =  LocationBySlotIdAndSlotRole f

------------------------------------------------------------------------------
-- EgestLocation
type EgestLocationNode f =  LocationBySlotId f

serverContext :: JsonLd.Context ServerContextFields
serverContext = wrap { "@language": Nothing
                     , "@base": Nothing
                     , "@vocab": Nothing
                     , address: JsonLd.Other "http://schema.rtsv2.llwn.com/Infrastructure/Address"
                     , pop: JsonLd.Other "http://schema.rtsv2.llnw.com/Infrastructure/PoP"
                     , region: JsonLd.Other "http://schema.rtsv2.llnw.com/Infrastructure/Region"
                     }

serverAddressContext :: JsonLd.Context ServerAddressContextFields
serverAddressContext = wrap { "@language": Nothing
                            , "@base": Nothing
                            , "@vocab": Nothing
                            , address: JsonLd.Other "http://schema.rtsv2.llwn.com/Infrastructure/Address"
                            }

deliverToContext :: JsonLd.Context DeliverToContextFields
deliverToContext = wrap { "@language": Nothing
                        , "@base": Nothing
                        , "@vocab": Nothing
                        , port: JsonLd.Other "http://schema.rtsv2.llwn.com/Network/Port"
                        , server: JsonLd.Other "http://schema.rtsv2.llnw.com/Infrastructure/Server"
                        }

egestServedLocationNode :: SlotId -> ServerAddress -> EgestServedLocationNode
egestServedLocationNode slotId serverAddress =
  wrap { resource: {address: serverAddress}
       , "@id": Just $ makeUrlAddr serverAddress (EgestStatsE slotId)
       , "@nodeType": Just "http://schema.rtsv2.llnw.com/EgestServedLocation"
       , "@context": Just $ serverAddressContext
       }

relayServedLocationNode :: SlotId -> SlotRole -> ServerAddress -> RelayServedLocationNode
relayServedLocationNode slotId slotRole serverAddress =
  wrap { resource: {address: serverAddress}
       , "@id": Just $ makeUrlAddr serverAddress (RelayStatsE slotId slotRole)
       , "@nodeType": Just "http://schema.rtsv2.llnw.com/RelayServedLocation"
       , "@context": Just $ serverAddressContext
       }

transPoPLeaderLocationNode :: Server -> TransPoPLeaderLocationNode
transPoPLeaderLocationNode server =
  wrap { resource: server
       , "@id": Just $ makeUrl server (TimedRoutesE (extractPoP server))
       , "@nodeType": Just "http://schema.rtsv2.llnw.com/TransPoPLocation"
       , "@context": Just $ serverContext
       }

activeIngestLocationNode :: SlotId -> SlotRole -> ProfileName -> ServerAddress -> ActiveIngestLocationNode
activeIngestLocationNode slotId slotRole profileName serverAddress =
  wrap { resource: { profileName
                   , serverAddress}
       , "@id": Just $ makeUrlAddr serverAddress (IngestInstanceE slotId slotRole profileName)
       , "@nodeType": Just "http://schema.rtsv2.llnw.com/ActiveIngestLocation"
       , "@context": Just $ wrap { "@language": Nothing
                                 , "@base": Nothing
                                 , "@vocab": Nothing
                                 , profileName: JsonLd.Other "http://schema.rtsv2.llwn.com/Media/ProfileName"
                                 , serverAddress: JsonLd.Other "http://schema.rtsv2.llnw.com/Infrastructure/ServerAddress"
                                 }
       }

downstreamRelayLocationNode :: SlotId -> SlotRole -> DeliverTo RelayServer -> DownstreamRelayLocationNode
downstreamRelayLocationNode slotId slotRole relay@{server} =
  wrap { resource: relay
       , "@id": Just $ makeUrl server (RelayStatsE slotId slotRole)
       , "@nodeType": Just "http://types.rtsv2.llnw.com/DownstreamRelayLocation"
       , "@context": Just deliverToContext
       }

aggregatorLocationNode :: SlotId -> SlotRole -> Server -> ServerNode
aggregatorLocationNode slotId slotRole server =
  wrap { resource: server
       , "@id": Just $ makeUrl server (IngestAggregatorE slotId slotRole)
       , "@nodeType": Just "http://types.rtsv2.llnw.com/AggregatorLocation"
       , "@context": Just $ serverContext
       }

relayLocationNode :: SlotId -> SlotRole -> Server -> ServerNode
relayLocationNode slotId slotRole server =
  wrap { resource: server
       , "@id": Just $ makeUrl server (RelayStatsE slotId slotRole)
       , "@nodeType": Just "http://types.rtsv2.llnw.com/RelayLocation"
       , "@context": Just serverContext
       }

egestLocationNode :: SlotId -> Server -> ServerNode
egestLocationNode slotId server =
  wrap { resource: server
       , "@id": Just $ makeUrl server (EgestStatsE slotId)
       , "@nodeType": Just "http://types.rtsv2.llnw.com/EgestLocation"
       , "@context": Just $ serverContext
       }

-- foo :: forall resourceType otherFields newtypeType. Newtype newtypeType { resource :: resourceType | otherFields } => newtypeType -> resourceType
-- foo = _.resource <<< unwrap

-- bar :: JsonLd.Node Server () -> String
-- bar server =
--   writeJSON server

-- baz :: String -> E (JsonLd.Node Server ())
-- baz s =
--   readJSON s
