module Shared.Rtsv2.JsonLd
       (
         ServerContextFields
       , ServerNode
       , serverContext

       , ServerAddressContextFields
       , ServerAddressNode
       , serverAddressContext

       , DeliverToContextFields
       , DeliverToNode
       , deliverToContext

       , LocationBySlotIdAndSlotRole
       , LocationBySlotId

       , ActiveIngestLocationContextFields
       , ActiveIngestLocation
       , ActiveIngestLocationNode
       , activeIngestLocationContext
       , activeIngestLocationNode

       , TransPoPLeaderLocationNode  -- == ServerNode
       , transPoPLeaderLocationNode

       , TimedRouteNeighbourContextFields
       , TimedRouteNeighbour
       , TimedRouteNeighbourNode
       , timedRouteNeighbourContext
       , timedRouteNeighbourNode

       , EgestServedLocationNode  -- == ServerAddressNode
       , egestServedLocationNode

       , DownstreamRelayLocationNode -- == DeliverToNode
       , downstreamRelayLocationNode

       , AggregatorLocationNode -- LocationBySlotIdAndSlotRole
       , aggregatorLocationNode

       , RelayLocationNode -- LocationBySlotIdAndSlotRole
       , relayLocationNode

       , EgestLocationNode -- LocationBySlotId
       , egestLocationNode

       , EgestStatsContextFields
       , EgestStats
       , EgestStatsNode
       , egestStatsContext
       , egestStatsNode

       , IntraPoPStateContextFields
       , IntraPoPState
       , IntraPoPStateNode
       , intraPoPStateContext
       , intraPoPStateNode

       , IngestAggregatorStateContextFields
       , IngestAggregatorState
       , IngestAggregatorStateNode
       , ingestAggregatorStateContext
       , ingestAggregatorStateNode

       , StreamRelayStateContextFields
       , StreamRelayState
       , StreamRelayStateNode
       , streamRelayStateContext
       , streamRelayStateNode

       , IngestStateContextFields
       , IngestState
       , IngestStateNode
       , ingestStateContext
       , ingestStateNode

       , _downstreamRelays
       , _activeProfiles
       , _profileName
       , _relaysServed
       , _port

       , module JsonLd
       ) where

import Prelude

import Data.Lens (Lens')
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Data.Symbol (SProxy(..))
import Shared.Common (Milliseconds)
import Shared.JsonLd ( Context
                     , ContextValue(..)
                     , ExpandedTermDefinition
                     , Node(..)
                     , NodeMetadata
                     , unwrapNode
                     , _unwrappedNode
                     , _id
                     , _resource
                     ) as JsonLd
import Shared.JsonLd (ContextDefinition(..))
import Shared.LlnwApiTypes (StreamDetails)
import Shared.Router.Endpoint (Endpoint(..), makePath, makeUrl, makeUrlAddr)
import Shared.Stream (ProfileName, SlotId, SlotRole)
import Shared.Types (DeliverTo, JsonLdContextType(..), PoPName, RelayServer, Server, ServerAddress)
import Shared.Types.Media.Types.Rtmp (RtmpClientMetadata)
import Shared.Types.Media.Types.SourceDetails (SourceInfo)

------------------------------------------------------------------------------
-- Server
type ServerContextFields = ( address :: JsonLd.ContextValue
                           , pop :: JsonLd.ContextValue
                           , region :: JsonLd.ContextValue
                           )

type ServerNode = JsonLd.Node Server ServerContextFields

serverContext :: JsonLd.Context ServerContextFields
serverContext = wrap { "@language": Nothing
                     , "@base": Nothing
                     , "@vocab": Nothing
                     , address: JsonLd.Other "http://schema.rtsv2.llwn.com/Infrastructure/Address"
                     , pop: JsonLd.Other "http://schema.rtsv2.llnw.com/Infrastructure/PoP"
                     , region: JsonLd.Other "http://schema.rtsv2.llnw.com/Infrastructure/Region"
                     }

------------------------------------------------------------------------------
-- ServerAddress
type ServerAddressContextFields = ( address :: JsonLd.ContextValue )

type ServerAddressNode = JsonLd.Node { address :: ServerAddress } ServerAddressContextFields

serverAddressContext :: JsonLd.Context ServerAddressContextFields
serverAddressContext = wrap { "@language": Nothing
                            , "@base": Nothing
                            , "@vocab": Nothing
                            , address: JsonLd.Other "http://schema.rtsv2.llwn.com/Infrastructure/Address"
                            }

------------------------------------------------------------------------------
-- DeliverTo
type DeliverToContextFields = ( port :: JsonLd.ContextValue
                              , server :: JsonLd.ContextValue
                              )

type DeliverToNode a = JsonLd.Node (DeliverTo a) DeliverToContextFields

deliverToContext :: JsonLd.Context DeliverToContextFields
deliverToContext = wrap { "@language": Nothing
                        , "@base": Nothing
                        , "@vocab": Nothing
                        , port: JsonLd.Other "http://schema.rtsv2.llwn.com/Network/Port"
                        , server: JsonLd.Other "http://schema.rtsv2.llnw.com/Infrastructure/Server"
                        }

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
type ActiveIngestLocationContextFields = ( profileName :: JsonLd.ContextValue
                                         , serverAddress :: JsonLd.ContextValue
                                         )

type ActiveIngestLocation = { profileName :: ProfileName
                            , serverAddress :: ServerAddress
                            }

type ActiveIngestLocationNode = JsonLd.Node ActiveIngestLocation ActiveIngestLocationContextFields

activeIngestLocationContext :: JsonLd.Context ActiveIngestLocationContextFields
activeIngestLocationContext =
  wrap { "@language": Nothing
       , "@base": Nothing
       , "@vocab": Nothing
       , profileName: JsonLd.Other "http://schema.rtsv2.llwn.com/Media/ProfileName"
       , serverAddress: JsonLd.Other "http://schema.rtsv2.llnw.com/Infrastructure/ServerAddress"
       }

activeIngestLocationNode :: SlotId -> SlotRole -> ProfileName -> ServerAddress -> ActiveIngestLocationNode
activeIngestLocationNode slotId slotRole profileName serverAddress =
  wrap { resource: { profileName
                   , serverAddress}
       , "@id": Just $ makeUrlAddr serverAddress (IngestInstanceE slotId slotRole profileName)
       , "@nodeType": Just "http://schema.rtsv2.llnw.com/ActiveIngestLocation"
       , "@context": Just $ ContextUrl $ makePath $ JsonLdContext ActiveIngestLocationContext
       }

------------------------------------------------------------------------------
-- TransPoPLeaderLocation
type TransPoPLeaderLocationNode = ServerNode

transPoPLeaderLocationNode :: Server -> TransPoPLeaderLocationNode
transPoPLeaderLocationNode server =
  wrap { resource: server
       , "@id": Just $ makeUrl server (TimedRoutesE)
       , "@nodeType": Just "http://schema.rtsv2.llnw.com/TransPoPLocation"
       , "@context": Just $ ContextUrl $ makePath $ JsonLdContext ServerContext
       }

------------------------------------------------------------------------------
-- TimedRouteNeighbourNode
type TimedRouteNeighbourContextFields = ( pop :: JsonLd.ContextValue )

type TimedRouteNeighbour f = { pop :: PoPName
                             , servers :: f ServerAddress }

type TimedRouteNeighbourNode f = JsonLd.Node (TimedRouteNeighbour f) TimedRouteNeighbourContextFields

timedRouteNeighbourContext :: JsonLd.Context TimedRouteNeighbourContextFields
timedRouteNeighbourContext = wrap { "@language": Nothing
                                  , "@base": Nothing
                                  , "@vocab": Nothing
                                  , pop: JsonLd.Other "http://schema.rtsv2.llnw.com/Infrastructure/PoP"
                                  }

timedRouteNeighbourNode :: forall f. Server -> TimedRouteNeighbour f -> TimedRouteNeighbourNode f
timedRouteNeighbourNode server neighbour@{pop} =
  wrap { resource: neighbour
       , "@id": Just $ makeUrl server (TimedRoutesForPoPE pop)
       , "@nodeType": Just "http://schema.rtsv2.llnw.com/TimedRouteNeighbour"
       , "@context": Just $ ContextUrl $ makePath $ JsonLdContext TimedRouteNeighbourContext
       }

------------------------------------------------------------------------------
-- EgestServedLocation
type EgestServedLocationNode = ServerAddressNode

egestServedLocationNode :: SlotId -> SlotRole -> ServerAddress -> EgestServedLocationNode
egestServedLocationNode slotId slotRole serverAddress =
  wrap { resource: {address: serverAddress}
       , "@id": Just $ makeUrlAddr serverAddress (EgestStatsE slotId slotRole)
       , "@nodeType": Just "http://schema.rtsv2.llnw.com/EgestServedLocation"
       , "@context": Just $ ContextUrl $ makePath $ JsonLdContext ServerAddressContext
       }

------------------------------------------------------------------------------
-- DownstreamRelayLocation
type DownstreamRelayLocationNode = DeliverToNode RelayServer

downstreamRelayLocationNode :: SlotId -> SlotRole -> DeliverTo RelayServer -> DownstreamRelayLocationNode
downstreamRelayLocationNode slotId slotRole relay@{server} =
  wrap { resource: relay
       , "@id": Just $ makeUrl server (RelayStatsE slotId slotRole)
       , "@nodeType": Just "http://types.rtsv2.llnw.com/DownstreamRelayLocation"
       , "@context": Just $ ContextUrl $ makePath $ JsonLdContext DeliverToContext
       }

------------------------------------------------------------------------------
-- AggregatorLocation
type AggregatorLocationNode f =  LocationBySlotIdAndSlotRole f

aggregatorLocationNode :: SlotId -> SlotRole -> Server -> ServerNode
aggregatorLocationNode slotId slotRole server =
  wrap { resource: server
       , "@id": Just $ makeUrl server (IngestAggregatorE slotId slotRole)
       , "@nodeType": Just "http://types.rtsv2.llnw.com/AggregatorLocation"
       , "@context": Just $ ContextUrl $ makePath $ JsonLdContext ServerContext
       }

------------------------------------------------------------------------------
-- RelayLocation
type RelayLocationNode f =  LocationBySlotIdAndSlotRole f

relayLocationNode :: SlotId -> SlotRole -> Server -> ServerNode
relayLocationNode slotId slotRole server =
  wrap { resource: server
       , "@id": Just $ makeUrl server (RelayStatsE slotId slotRole)
       , "@nodeType": Just "http://types.rtsv2.llnw.com/RelayLocation"
       , "@context": Just $ ContextUrl $ makePath $ JsonLdContext ServerContext
       }

------------------------------------------------------------------------------
-- EgestLocation
type EgestLocationNode f =  LocationBySlotIdAndSlotRole f

egestLocationNode :: SlotId -> SlotRole -> Server -> ServerNode
egestLocationNode slotId slotRole server =
  wrap { resource: server
       , "@id": Just $ makeUrl server (EgestStatsE slotId slotRole)
       , "@nodeType": Just "http://types.rtsv2.llnw.com/EgestLocation"
       , "@context": Just $ ContextUrl $ makePath $ JsonLdContext ServerContext
       }

------------------------------------------------------------------------------
-- EgestStats
type EgestStatsContextFields = ( clientCount :: JsonLd.ContextValue )

type EgestStats = { clientCount :: Int }

type EgestStatsNode = JsonLd.Node EgestStats EgestStatsContextFields

egestStatsContext :: JsonLd.Context EgestStatsContextFields
egestStatsContext = wrap { "@language": Nothing
                         , "@base": Nothing
                         , "@vocab": Nothing
                         , clientCount: JsonLd.Other "http://schema.rtsv2.llnw.com/Counter"
                         }

egestStatsNode :: SlotId -> SlotRole -> Server -> EgestStats -> EgestStatsNode
egestStatsNode slotId slotRole server stats =
  wrap { resource: stats
       , "@id": Just $ makeUrl server (EgestStatsE slotId slotRole)
       , "@nodeType": Just "http://types.rtsv2.llnw.com/Egest"
       , "@context": Just $ ContextUrl $ makePath $ JsonLdContext EgestStatsContext
       }

------------------------------------------------------------------------------
-- IntraPoPState
type IntraPoPStateContextFields = ( aggregatorLocations :: JsonLd.ContextValue
                                  , relayLocations :: JsonLd.ContextValue
                                  , egestLocations :: JsonLd.ContextValue
                                  , currentTransPoPLeader :: JsonLd.ContextValue
                                  )
type IntraPoPState f
  = { aggregatorLocations :: f (AggregatorLocationNode f)
    , relayLocations      :: f (RelayLocationNode f)
    , egestLocations      :: f (EgestLocationNode f)
    , currentTransPoPLeader :: Maybe (TransPoPLeaderLocationNode)
    }

type IntraPoPStateNode f = JsonLd.Node (IntraPoPState f) IntraPoPStateContextFields

intraPoPStateContext :: JsonLd.Context IntraPoPStateContextFields
intraPoPStateContext = wrap { "@language": Nothing
                            , "@base": Nothing
                            , "@vocab": Nothing
                            , aggregatorLocations: JsonLd.Other "http://schema.rtsv2.llnw.com/Infrastructure/Locations/Aggegrator"
                            , relayLocations: JsonLd.Other "http://schema.rtsv2.llnw.com/Infrastructure/Locations/Relay"
                            , egestLocations: JsonLd.Other "http://schema.rtsv2.llnw.com/Infrastructure/Locations/Egest"
                            , currentTransPoPLeader: JsonLd.Other "http://schema.rtsv2.llnw.com/Infrastructure/Locations/TransPoPLeader"
                            }

intraPoPStateNode :: forall f. IntraPoPState f -> Server -> IntraPoPStateNode f
intraPoPStateNode state server =
  wrap { resource: state
       , "@id": Just $ makeUrl server ServerStateE
       , "@nodeType": Just "http://types.rtsv2.llnw.com/IntraPoP"
       , "@context": Just $ ContextUrl $ makePath $ JsonLdContext EgestStatsContext
       }

------------------------------------------------------------------------------
-- IngestAggregatorState
type IngestAggregatorStateContextFields = ( role :: JsonLd.ContextValue
                                          , streamDetails :: JsonLd.ContextValue
                                          , activeProfiles :: JsonLd.ContextValue
                                          , downstreamRelays :: JsonLd.ContextValue
                                          )
type IngestAggregatorState f
   = { role :: SlotRole
     , streamDetails :: StreamDetails
     , activeProfiles :: f ActiveIngestLocationNode
     , downstreamRelays :: f DownstreamRelayLocationNode
     }

type IngestAggregatorStateNode f = JsonLd.Node (IngestAggregatorState f) IngestAggregatorStateContextFields

ingestAggregatorStateContext :: JsonLd.Context IngestAggregatorStateContextFields
ingestAggregatorStateContext = wrap { "@language": Nothing
                                    , "@base": Nothing
                                    , "@vocab": Nothing
                                    , role: JsonLd.Other "http://schema.rtsv2.llnw.com/Role"
                                    , streamDetails: JsonLd.Other "http://schema.rtsv2.llnw.com/Media/StreamDetails"
                                    , activeProfiles: JsonLd.Other "http://schema.rtsv2.llnw.com/Infrastructure/Locations/Ingest"
                                    , downstreamRelays: JsonLd.Other "http://schema.rtsv2.llnw.com/Infrastructure/Locations/Relay"
                                    }
ingestAggregatorStateNode :: forall f. SlotId -> IngestAggregatorState f -> Server -> IngestAggregatorStateNode f
ingestAggregatorStateNode slotId state@{role: slotRole} server =
  wrap { resource: state
       , "@id": Just $ makeUrl server (IngestAggregatorE slotId slotRole)
       , "@nodeType": Just "http://types.rtsv2.llnw.com/IngestAggregator"
       , "@context": Just $ ContextUrl $ makePath $ JsonLdContext EgestStatsContext
       }

------------------------------------------------------------------------------
-- StreamRelayState
type StreamRelayStateContextFields = ( role :: JsonLd.ContextValue
                                     , egestsServed :: JsonLd.ContextValue
                                     , relaysServed :: JsonLd.ContextValue
                                     )
type StreamRelayState f
  = { role :: SlotRole
    , egestsServed :: f EgestServedLocationNode
    , relaysServed :: f DownstreamRelayLocationNode
    }

type StreamRelayStateNode f = JsonLd.Node (StreamRelayState f) StreamRelayStateContextFields

streamRelayStateContext :: JsonLd.Context StreamRelayStateContextFields
streamRelayStateContext = wrap { "@language": Nothing
                               , "@base": Nothing
                               , "@vocab": Nothing
                               , role: JsonLd.Other "http://schema.rtsv2.llnw.com/Role"
                               , egestsServed: JsonLd.Other "http://schema.rtsv2.llnw.com/Infrastructure/Locations/Egest"
                               , relaysServed: JsonLd.Other "http://schema.rtsv2.llnw.com/Infrastructure/Locations/Relay"
                               }

streamRelayStateNode :: forall f. SlotId -> StreamRelayState f -> Server -> StreamRelayStateNode f
streamRelayStateNode slotId state@{role: slotRole} server =
  wrap { resource: state
       , "@id": Just $ makeUrl server (RelayStatsE slotId slotRole)
       , "@nodeType": Just "http://types.rtsv2.llnw.com/IngestAggregator"
       , "@context": Just $ ContextUrl $ makePath $ JsonLdContext StreamRelayStateContext
       }

------------------------------------------------------------------------------
-- IngestState
type IngestStateContextFields = ( ingestStartedTime :: JsonLd.ContextValue
                                , remoteAddress :: JsonLd.ContextValue
                                , remotePort :: JsonLd.ContextValue
                                , rtmpClientMetadata :: JsonLd.ContextValue
                                , sourceInfo :: JsonLd.ContextValue
                                )

type IngestState f
  = { ingestStartedTime :: Milliseconds
    , remoteAddress :: String
    , remotePort :: Int
    , rtmpClientMetadata :: Maybe (RtmpClientMetadata f)
    , sourceInfo :: Maybe (SourceInfo f)
    }

type IngestStateNode f = JsonLd.Node (IngestState f) IngestStateContextFields

ingestStateContext :: JsonLd.Context IngestStateContextFields
ingestStateContext = wrap { "@language": Nothing
                          , "@base": Nothing
                          , "@vocab": Nothing
                          , ingestStartedTime: JsonLd.Other "http://schema.rtsv2.llnw.com/UTCTimestamp"
                          , remoteAddress: JsonLd.Other "http://schema.rtsv2.llnw.com/Network/Address"
                          , remotePort: JsonLd.Other "http://schema.rtsv2.llwn.com/Network/Port"
                          , rtmpClientMetadata: JsonLd.Other "http://schema.rtsv2.llnw.com/Media/Rtmp/Metadata"
                          , sourceInfo: JsonLd.Other "http://schema.rtsv2.llnw.com/Media/SourceInfo"
                          }

ingestStateNode :: forall f. SlotId -> SlotRole -> ProfileName -> IngestState f -> Server -> IngestStateNode f
ingestStateNode slotId slotRole profileName state server =
  wrap { resource: state
       , "@id": Just $ makeUrl server (IngestInstanceE slotId slotRole profileName)
       , "@nodeType": Just "http://types.rtsv2.llnw.com/Ingest"
       , "@context": Just $ ContextUrl $ makePath $ JsonLdContext IngestStateContext
       }

--------------------------------------------------------------------------------
-- Lenses
--------------------------------------------------------------------------------
_downstreamRelays :: forall a b. Lens' {downstreamRelays :: a | b} a
_downstreamRelays = prop (SProxy :: SProxy "downstreamRelays")

_activeProfiles :: forall a b. Lens' {activeProfiles :: a | b} a
_activeProfiles = prop (SProxy :: SProxy "activeProfiles")

_profileName :: forall a b. Lens' {profileName :: a | b} a
_profileName = prop (SProxy :: SProxy "profileName")

_relaysServed :: forall a b. Lens' {relaysServed :: a | b} a
_relaysServed = prop (SProxy :: SProxy "relaysServed")

_port :: forall a b. Lens' {port :: a | b} a
_port = prop (SProxy :: SProxy "port")

-- foo :: forall resourceType otherFields newtypeType. Newtype newtypeType { resource :: resourceType | otherFields } => newtypeType -> resourceType
-- foo = _.resource <<< unwrap

-- bar :: JsonLd.Node Server () -> String
-- bar server =
--   writeJSON server

-- baz :: String -> E (JsonLd.Node Server ())
-- baz s =
--   readJSON s
