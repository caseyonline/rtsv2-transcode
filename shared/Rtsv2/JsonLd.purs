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
       , EgestSessionStats(..)
       , EgestRtmpSessionStats
       , EgestRtcSessionStats
       , EgestStatsNode
       , statsSessionId
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

       , NodeManagerStateContextFields
       , NodeManagerState
       , NodeManagerStateNode
       , nodeManagerStateContext
       , nodeManagerStateNode

       , HealthContextFields
       , Health
       , HealthNode
       , healthContext
       , healthNode

       , _downstreamRelays
       , _activeProfiles
       , _profileName
       , _relaysServed
       , _port
       , _timestamp
       , _sessions

       , module JsonLd
       ) where

import Prelude

import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens (Lens')
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Data.Symbol (SProxy(..))
import Effect (Effect)
import Foreign (ForeignError(..), MultipleErrors, fail)
import Kishimen (genericSumToVariant, variantToGenericSum)
import Record as Record
import Shared.Common (Alert, Milliseconds, CacheUtilization)
import Shared.JsonLd (Context, ContextValue(..), ExpandedTermDefinition, Node(..), NodeMetadata, unwrapNode, _unwrappedNode, _id, _resource) as JsonLd
import Shared.JsonLd (ContextDefinition(..))
import Shared.Rtsv2.LlnwApiTypes (StreamDetails)
import Shared.Rtsv2.Router.Endpoint.Support as Support
import Shared.Rtsv2.Stream (EgestKey, ProfileName, SlotId, SlotRole)
import Shared.Rtsv2.Types (CanaryState, CurrentLoad, DeliverTo, JsonLdContextType(..), PoPName, RelayServer, RunState, Server, ServerAddress)
import Shared.Rtsv2.Types as Types
import Shared.Types.Media.Types.Rtmp (RtmpClientMetadata)
import Shared.Types.Media.Types.SourceDetails (SourceInfo)
import Simple.JSON (class ReadForeign, class WriteForeign, readImpl, writeImpl)

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

activeIngestLocationNode :: SlotId -> SlotRole -> ProfileName -> ServerAddress -> Effect ActiveIngestLocationNode
activeIngestLocationNode slotId slotRole profileName serverAddress = do
  id <- Support.makeUrlAddr serverAddress (Support.IngestInstanceE slotId slotRole profileName)
  pure $ wrap { resource: { profileName
                          , serverAddress}
              , "@id": Just id
              , "@nodeType": Just "http://schema.rtsv2.llnw.com/ActiveIngestLocation"
              , "@context": Just $ ContextUrl $ Support.makePath $ Support.JsonLdContext ActiveIngestLocationContext
              }

------------------------------------------------------------------------------
-- TransPoPLeaderLocation
type TransPoPLeaderLocationNode = ServerNode

transPoPLeaderLocationNode :: Server -> Effect TransPoPLeaderLocationNode
transPoPLeaderLocationNode server = do
  id <- Support.makeUrl server (Support.TimedRoutesE)
  pure $ wrap { resource: server
              , "@id": Just id
              , "@nodeType": Just "http://schema.rtsv2.llnw.com/TransPoPLocation"
              , "@context": Just $ ContextUrl $ Support.makePath $ Support.JsonLdContext ServerContext
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

timedRouteNeighbourNode :: forall f. Server -> TimedRouteNeighbour f -> Effect (TimedRouteNeighbourNode f)
timedRouteNeighbourNode server neighbour@{pop} = do
  id <- Support.makeUrl server (Support.TimedRoutesForPoPE pop)
  pure $ wrap { resource: neighbour
              , "@id": Just $ id
              , "@nodeType": Just "http://schema.rtsv2.llnw.com/TimedRouteNeighbour"
              , "@context": Just $ ContextUrl $ Support.makePath $ Support.JsonLdContext TimedRouteNeighbourContext
              }

------------------------------------------------------------------------------
-- EgestServedLocation
type EgestServedLocationNode = ServerAddressNode

egestServedLocationNode :: SlotId -> SlotRole -> ServerAddress -> Effect EgestServedLocationNode
egestServedLocationNode slotId slotRole serverAddress = do
  id <- Support.makeUrlAddr serverAddress (Support.EgestStatsE slotId slotRole)
  pure $ wrap { resource: {address: serverAddress}
              , "@id": Just id
              , "@nodeType": Just "http://schema.rtsv2.llnw.com/EgestServedLocation"
              , "@context": Just $ ContextUrl $ Support.makePath $ Support.JsonLdContext ServerAddressContext
              }

------------------------------------------------------------------------------
-- DownstreamRelayLocation
type DownstreamRelayLocationNode = DeliverToNode RelayServer

downstreamRelayLocationNode :: SlotId -> SlotRole -> DeliverTo RelayServer -> Effect DownstreamRelayLocationNode
downstreamRelayLocationNode slotId slotRole relay@{server} = do
  id <- Support.makeUrl server (Support.RelayStatsE slotId slotRole)
  pure $ wrap { resource: relay
              , "@id": Just id
              , "@nodeType": Just "http://types.rtsv2.llnw.com/DownstreamRelayLocation"
              , "@context": Just $ ContextUrl $ Support.makePath $ Support.JsonLdContext DeliverToContext
              }

------------------------------------------------------------------------------
-- AggregatorLocation
type AggregatorLocationNode f =  LocationBySlotIdAndSlotRole f

aggregatorLocationNode :: SlotId -> SlotRole -> Server -> Effect ServerNode
aggregatorLocationNode slotId slotRole server = do
  id <- Support.makeUrl server (Support.IngestAggregatorE slotId slotRole)
  pure $ wrap { resource: server
              , "@id": Just id
              , "@nodeType": Just "http://types.rtsv2.llnw.com/AggregatorLocation"
              , "@context": Just $ ContextUrl $ Support.makePath $ Support.JsonLdContext ServerContext
              }

------------------------------------------------------------------------------
-- RelayLocation
type RelayLocationNode f =  LocationBySlotIdAndSlotRole f

relayLocationNode :: SlotId ->  SlotRole -> Server -> Effect ServerNode
relayLocationNode slotId slotRole server = do
  id <- Support.makeUrl server (Support.RelayStatsE slotId slotRole)
  pure $ wrap { resource: server
              , "@id": Just id
              , "@nodeType": Just "http://types.rtsv2.llnw.com/RelayLocation"
              , "@context": Just $ ContextUrl $ Support.makePath $ Support.JsonLdContext ServerContext
              }

------------------------------------------------------------------------------
-- EgestLocation
type EgestLocationNode f =  LocationBySlotIdAndSlotRole f

egestLocationNode :: SlotId -> SlotRole -> Server -> Effect ServerNode
egestLocationNode slotId slotRole server = do
  id <- Support.makeUrl server (Support.EgestStatsE slotId slotRole)
  pure $ wrap { resource: server
              , "@id": Just id
              , "@nodeType": Just "http://types.rtsv2.llnw.com/EgestLocation"
              , "@context": Just $ ContextUrl $ Support.makePath $ Support.JsonLdContext ServerContext
              }

------------------------------------------------------------------------------
-- EgestStats
type EgestStatsContextFields = ( timestamp :: JsonLd.ContextValue
                               , egestKey :: JsonLd.ContextValue
                               , clientCount :: JsonLd.ContextValue
                               , totalClientCount :: JsonLd.ContextValue
                               , sessions :: JsonLd.ContextValue
                               )

data EgestSessionStats = EgestRtcSessionStats EgestRtcSessionStats
                       | EgestRtmpSessionStats EgestRtmpSessionStats

derive instance eqEgestSessionStats :: Eq EgestSessionStats
derive instance genericEgestSessionStats :: Generic EgestSessionStats _
instance showEgestSessionStats :: Show EgestSessionStats where show = genericShow

instance readForeignEgestSessionStats :: ReadForeign EgestSessionStats where
  readImpl o = do
    rec :: { connectionType :: String } <- readImpl o
    case rec.connectionType of
      "webrtc" -> EgestRtcSessionStats <$> readImpl o
      "rtmp" -> EgestRtmpSessionStats <$> readImpl o
      _ -> fail $ ForeignError $ "Bad connection type " <> rec.connectionType

instance writeForeignEgestSessionStats :: WriteForeign EgestSessionStats where
  writeImpl (EgestRtcSessionStats stats) =
    writeImpl $ Record.insert (SProxy :: SProxy "connectionType") "webrtc" stats
  writeImpl (EgestRtmpSessionStats stats) =
    writeImpl $ Record.insert (SProxy :: SProxy "connectionType") "rtmp" stats

type EgestRtcSessionStats = { sessionId :: String
                         , audioPacketsSent :: Int
                         , audioOctetsSent :: Int
                         , videoPacketsSent :: Int
                         , videoOctetsSent :: Int
                         }

type EgestRtmpSessionStats = { sessionId :: String
                             , octetsSent :: Int
                             , octetsReceived :: Int
                             }

statsSessionId :: EgestSessionStats -> String
statsSessionId (EgestRtmpSessionStats {sessionId}) = sessionId
statsSessionId (EgestRtcSessionStats {sessionId}) = sessionId


type EgestStats f = { timestamp :: Milliseconds
                    , egestKey :: EgestKey
                    , clientCount :: Int
                    , totalClientCount :: Int
                    , sessions :: f EgestSessionStats
                    }

type EgestStatsNode f = JsonLd.Node (EgestStats f) EgestStatsContextFields

egestStatsContext :: JsonLd.Context EgestStatsContextFields
egestStatsContext = wrap { "@language": Nothing
                         , "@base": Nothing
                         , "@vocab": Nothing
                         , timestamp: JsonLd.Other "http://schema.rtsv2.llnw.com/UTCTimestamp"
                         , egestKey: JsonLd.Other "http://schema.rtsv2.llnw.com/EgestKey"
                         , clientCount: JsonLd.Other "http://schema.rtsv2.llnw.com/Counter"
                         , totalClientCount: JsonLd.Other "http://schema.rtsv2.llnw.com/Counter"
                         , sessions: JsonLd.Other "http://schema.rtsv2.llnw.com/EgestSessions"
                         }

egestStatsNode :: forall f. SlotId -> SlotRole -> Server -> EgestStats f -> Effect (EgestStatsNode f)
egestStatsNode slotId slotRole server stats = do
  id <- Support.makeUrl server (Support.EgestStatsE slotId slotRole)
  pure$ wrap { resource: stats
             , "@id": Just id
             , "@nodeType": Just "http://types.rtsv2.llnw.com/EgestStats"
             , "@context": Just $ ContextUrl $ Support.makePath $ Support.JsonLdContext EgestStatsContext
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

intraPoPStateNode :: forall f. IntraPoPState f -> Server -> Effect (IntraPoPStateNode f)
intraPoPStateNode state server = do
  id <- Support.makeUrl server Support.ServerStateE
  pure $ wrap { resource: state
              , "@id": Just id
              , "@nodeType": Just "http://types.rtsv2.llnw.com/IntraPoP"
              , "@context": Just $ ContextUrl $ Support.makePath $ Support.JsonLdContext EgestStatsContext
              }

------------------------------------------------------------------------------
-- IngestAggregatorState
type IngestAggregatorStateContextFields = ( role :: JsonLd.ContextValue
                                          , streamDetails :: JsonLd.ContextValue
                                          , activeProfiles :: JsonLd.ContextValue
                                          , downstreamRelays :: JsonLd.ContextValue
                                          , hlsPublish :: JsonLd.ContextValue
                                          , clientCount :: JsonLd.ContextValue
                                          )
type IngestAggregatorState f
   = { role :: SlotRole
     , streamDetails :: StreamDetails
     , activeProfiles :: f ActiveIngestLocationNode
     , downstreamRelays :: f DownstreamRelayLocationNode
     , hlsPublish :: Boolean
     , clientCount :: Int
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
                                    , hlsPublish: JsonLd.Other "http://schema.rtsv2.llnw.com/Boolean"
                                    , clientCount: JsonLd.Other "http://schema.rtsv2.llnw.com/Counter"
                                    }
ingestAggregatorStateNode :: forall f. SlotId -> IngestAggregatorState f -> Server -> Effect (IngestAggregatorStateNode f)
ingestAggregatorStateNode slotId state@{role: slotRole} server = do
  id <- Support.makeUrl server (Support.IngestAggregatorE slotId slotRole)
  pure $ wrap { resource: state
              , "@id": Just id
              , "@nodeType": Just "http://types.rtsv2.llnw.com/IngestAggregator"
              , "@context": Just $ ContextUrl $ Support.makePath $ Support.JsonLdContext IngestAggregatorStateContext
              }

------------------------------------------------------------------------------
-- StreamRelayState
type StreamRelayStateContextFields = ( role :: JsonLd.ContextValue
                                     , egestsServed :: JsonLd.ContextValue
                                     , relaysServed :: JsonLd.ContextValue
                                     , downstreamClientCount :: JsonLd.ContextValue
                                     , totalClientCount :: JsonLd.ContextValue
                                     )
type StreamRelayState f
  = { role :: SlotRole
    , egestsServed :: f EgestServedLocationNode
    , relaysServed :: f DownstreamRelayLocationNode
    , downstreamClientCount :: Int
    , totalClientCount :: Int
    }

type StreamRelayStateNode f = JsonLd.Node (StreamRelayState f) StreamRelayStateContextFields

streamRelayStateContext :: JsonLd.Context StreamRelayStateContextFields
streamRelayStateContext = wrap { "@language": Nothing
                               , "@base": Nothing
                               , "@vocab": Nothing
                               , role: JsonLd.Other "http://schema.rtsv2.llnw.com/Role"
                               , egestsServed: JsonLd.Other "http://schema.rtsv2.llnw.com/Infrastructure/Locations/Egest"
                               , relaysServed: JsonLd.Other "http://schema.rtsv2.llnw.com/Infrastructure/Locations/Relay"
                               , downstreamClientCount: JsonLd.Other "http://schema.rtsv2.llnw.com/Infrastructure/Locations/Counter"
                               , totalClientCount: JsonLd.Other "http://schema.rtsv2.llnw.com/Infrastructure/Locations/Counter"
                               }

streamRelayStateNode :: forall f. SlotId -> StreamRelayState f -> Server -> Effect (StreamRelayStateNode f)
streamRelayStateNode slotId state@{role: slotRole} server = do
  id <- Support.makeUrl server (Support.RelayStatsE slotId slotRole)
  pure $ wrap { resource: state
              , "@id": Just id
              , "@nodeType": Just "http://types.rtsv2.llnw.com/StreamRelay"
              , "@context": Just $ ContextUrl $ Support.makePath $ Support.JsonLdContext StreamRelayStateContext
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

ingestStateNode :: forall f. SlotId -> SlotRole -> ProfileName -> IngestState f -> Server -> Effect (IngestStateNode f)
ingestStateNode slotId slotRole profileName state server = do
  id <- Support.makeUrl server (Support.IngestInstanceE slotId slotRole profileName)
  pure $ wrap { resource: state
              , "@id": Just id
              , "@nodeType": Just "http://types.rtsv2.llnw.com/Ingest"
              , "@context": Just $ ContextUrl $ Support.makePath $ Support.JsonLdContext IngestStateContext
              }

--------------------------------------------------------------------------------
-- NodeManager
type NodeManagerStateContextFields = ( runState :: JsonLd.ContextValue
                                     , canaryState :: JsonLd.ContextValue
                                     , ingests :: JsonLd.ContextValue
                                     , ingestAggregators :: JsonLd.ContextValue
                                     , streamRelays :: JsonLd.ContextValue
                                     , egests :: JsonLd.ContextValue
                                     )

type NodeManagerState
  = { runState :: RunState
    , canaryState :: CanaryState
    , ingests :: Int
    , ingestAggregators :: Int
    , streamRelays :: Int
    , egests :: Int
    }

type NodeManagerStateNode = JsonLd.Node NodeManagerState NodeManagerStateContextFields

nodeManagerStateContext :: JsonLd.Context NodeManagerStateContextFields
nodeManagerStateContext = wrap { "@language": Nothing
                               , "@base": Nothing
                               , "@vocab": Nothing
                               , runState: JsonLd.Other "http://schema.rtsv2.llnw.com/RunState"
                               , canaryState: JsonLd.Other "http://schema.rtsv2.llnw.com/CanaryState"
                               , ingests: JsonLd.Other "http://schema.rtsv2.llnw.com/AgentCount"
                               , ingestAggregators: JsonLd.Other "http://schema.rtsv2.llnw.com/AgentCount"
                               , streamRelays: JsonLd.Other "http://schema.rtsv2.llnw.com/AgentCount"
                               , egests: JsonLd.Other "http://schema.rtsv2.llnw.com/AgentCount"
                               }

nodeManagerStateNode :: NodeManagerState -> Server -> NodeManagerStateNode
nodeManagerStateNode state server =
  wrap { resource: state
       , "@id": Nothing -- TODO Just $ Support.makeUrl server (Support.IngestInstanceE slotId slotRole profileName)
       , "@nodeType": Just "http://types.rtsv2.llnw.com/NodeManager"
       , "@context": Just $ ContextUrl $ Support.makePath $ Support.JsonLdContext NodeManagerStateContext
       }

--------------------------------------------------------------------------------
-- Health
type HealthContextFields = ( intraPoPHealth :: JsonLd.ContextValue
                           , transPoPHealth :: JsonLd.ContextValue
                           , currentTransPoP :: JsonLd.ContextValue
                           , load :: JsonLd.ContextValue
                           , nodeManager :: JsonLd.ContextValue
                           , alerts :: JsonLd.ContextValue
                           , slotLookupCacheUtilization :: JsonLd.ContextValue
                           )

type Health f
  = { intraPoPHealth :: Types.Health
    , transPoPHealth :: Types.Health
    , currentTransPoP :: Maybe ServerAddress
    , load :: CurrentLoad
    , nodeManager :: NodeManagerStateNode
    , alerts :: f Alert
    , slotLookupCacheUtilization :: CacheUtilization
    }

type HealthNode f = JsonLd.Node (Health f) HealthContextFields

healthContext :: JsonLd.Context HealthContextFields
healthContext = wrap { "@language": Nothing
                     , "@base": Nothing
                     , "@vocab": Nothing
                     , intraPoPHealth: JsonLd.Other "http://schema.rtsv2.llnw.com/Health"
                     , transPoPHealth: JsonLd.Other "http://schema.rtsv2.llnw.com/Health"
                     , currentTransPoP: JsonLd.Other "http://schema.rtsv2.llnw.com/Infrastructure/Locations/TransPoP"
                     , load: JsonLd.Other "http://schema.rtsv2.llnw.com/Load"
                     , nodeManager: JsonLd.Other "http://schema.rtsv2.llnw.com/NodeManager"
                     , alerts: JsonLd.Other "http://schema.rtsv2.llnw.com/Alert"
                     , slotLookupCacheUtilization: JsonLd.Other "http://schema.rtsv2.llnw.com/CacheUtilization"
                     }

healthNode :: forall f. Functor f => Health f -> Server -> Effect (HealthNode f)
healthNode state server = do
  id <- Support.makeUrl server Support.HealthCheckE
  pure $ wrap { resource: state
              , "@id": Just id
              , "@nodeType": Just "http://types.rtsv2.llnw.com/Health"
              , "@context": Just $ ContextUrl $ Support.makePath $ Support.JsonLdContext HealthContext
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

_timestamp :: forall a b. Lens' {timestamp :: a | b} a
_timestamp = prop (SProxy :: SProxy "timestamp")

_sessions :: forall a b. Lens' {sessions :: a | b} a
_sessions = prop (SProxy :: SProxy "sessions")
