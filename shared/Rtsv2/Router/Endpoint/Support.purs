module Shared.Rtsv2.Router.Endpoint.Support where

import Prelude hiding ((/))

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Newtype (class Newtype, wrap)
import Effect (Effect)
import Routing.Duplex (RouteDuplex', path, print, rest, root, segment)
import Routing.Duplex.Generic (noArgs, sum)
import Routing.Duplex.Generic.Syntax ((/))
import Rtsv2.Config as Config
import Shared.Common (Url)
import Shared.Rtsv2.Router.Endpoint.Combinators (contextType, popName, profileName, shortName, slotId, slotRole, streamName)
import Shared.Rtsv2.Stream (ProfileName, RtmpShortName, RtmpStreamName, SlotId, SlotRole)
import Shared.Rtsv2.Types (JsonLdContextType, PoPName, ServerAddress(..), extractAddress)

data Endpoint
  =
  -- Support
    VMMetricsE
  | TimedRoutesE
  | TimedRoutesForPoPE PoPName
  | HealthCheckE
  | CanaryE
  | RunStateE
  | ServerStateE
  | SlotStateE SlotId
  | PoPDefinitionE
  | JsonLdContext JsonLdContextType
  | EgestStatsE SlotId SlotRole
  | EgestInstancesMetricsE
  | RelayStatsE SlotId SlotRole
  | IngestAggregatorE SlotId SlotRole
  | IngestAggregatorPlayerE SlotId SlotRole
  | IngestAggregatorPlayerJsE SlotId SlotRole (Array String)
  | IngestAggregatorActiveIngestsPlayerE SlotId SlotRole ProfileName
  | IngestAggregatorActiveIngestsPlayerJsE SlotId SlotRole ProfileName (Array String)
  | IngestAggregatorActiveIngestsPlayerControlE SlotId SlotRole ProfileName
  | IngestAggregatorsE
  | IngestInstancesMetricsE
  | IngestInstanceE SlotId SlotRole ProfileName
  | ClientAppAssetsE (Array String)
  | ClientAppRouteHTMLE

  | CanaryStreamDiscoveryE String String
  | CanaryClientPlayerE SlotId SlotRole
  | CanaryClientPlayerControlE SlotId SlotRole
  | CanaryClientPlayerAssetsE SlotId SlotRole (Array String)

  | CanaryClientWebRTCIngestE RtmpShortName RtmpStreamName
  | CanaryClientWebRTCIngestControlE RtmpShortName RtmpStreamName
  | CanaryClientWebRTCIngestAssetsE RtmpShortName RtmpStreamName (Array String)

derive instance genericEndpoint :: Generic Endpoint _

instance showEndpoint :: Show Endpoint where
  show = genericShow

-- | Our codec will cause a compile-time error if we fail to handle any of our route cases.
endpoint :: RouteDuplex' Endpoint
endpoint = root $ sum
  {
  -- Support
    "VMMetricsE"                                       : "support" / "vm" / "metrics" / noArgs
  , "TimedRoutesE"                                     : "support" / "timedRoutes" / noArgs
  , "TimedRoutesForPoPE"                               : "support" / "timedRoutes" / popName segment
  , "HealthCheckE"                                     : "support" / "healthCheck" / noArgs
  , "CanaryE"                                          : "support" / "canary" / noArgs
  , "RunStateE"                                        : "support" / "runState" / noArgs
  , "ServerStateE"                                     : "support" / "state" / noArgs
  , "SlotStateE"                                       : "support" / "state" / "slot" / slotId segment
  , "PoPDefinitionE"                                   : "support" / "popDefinition" / noArgs
  , "JsonLdContext"                                    : "support" / "jsonld" / contextType segment
  , "EgestStatsE"                                      : "support" / "egest" / slotId segment / slotRole segment
  , "EgestInstancesMetricsE"                           : "support" / "egest" / path "metrics" noArgs
  , "RelayStatsE"                                      : "support" / "relay" / slotId segment / slotRole segment  -- TODO - stats vs status
  , "IngestAggregatorE"                                : "support" / "ingestAggregator" / slotId segment / slotRole segment

  , "IngestAggregatorPlayerE"                          : "support" / "ingestAggregator" / slotId segment / slotRole segment / "player"
  , "IngestAggregatorPlayerJsE"                        : "support" / "ingestAggregator" / slotId segment / slotRole segment / "js" / rest
  , "IngestAggregatorActiveIngestsPlayerE"             : "support" / "ingestAggregator" / slotId segment / slotRole segment / "activeIngests" / profileName segment / "player"
  , "IngestAggregatorActiveIngestsPlayerJsE"           : "support" / "ingestAggregator" / slotId segment / slotRole segment / "activeIngests" / profileName segment / "js" / rest
  , "IngestAggregatorActiveIngestsPlayerControlE"      : "support" / "ingestAggregator" / slotId segment / slotRole segment / "activeIngests" / profileName segment / "control" -- URL duplicated in Web.purs
  , "IngestAggregatorsE"                               : "support" / "ingestAggregator" / noArgs
  , "IngestInstancesMetricsE"                          : "support" / "ingest" / "metrics" / noArgs
  , "IngestInstanceE"                                  : "support" / "ingest" / slotId segment / slotRole segment / profileName segment

  , "ClientAppAssetsE"                                 : "support" / "assets" / rest
  , "ClientAppRouteHTMLE"                              : "support" / noArgs

  , "CanaryStreamDiscoveryE"                           : "support" / "canary" / "discovery" / "v1" / segment / segment

  , "CanaryClientPlayerE"                              : "support" / "canary" / "client" / slotId segment / slotRole segment / "player"
  , "CanaryClientPlayerControlE"                       : "support" / "canary" / "client" / slotId segment / slotRole segment / "session" -- URL duplicated in Web.purs
  , "CanaryClientPlayerAssetsE"                        : "support" / "canary" / "client" / slotId segment / slotRole segment / rest

  , "CanaryClientWebRTCIngestE"                        : "support" / "canary" / "ingest" / shortName segment / streamName segment / "ingest"
  , "CanaryClientWebRTCIngestControlE"                 : "support" / "canary" / "ingest" / shortName segment / streamName segment / "session"
  , "CanaryClientWebRTCIngestAssetsE"                  : "support" / "canary" / "ingest" / shortName segment / streamName segment / rest
}

makePath :: Endpoint -> String
makePath ep = print endpoint ep

makeUrl
  :: forall r a. Newtype a { address :: ServerAddress | r }
  => a -> Endpoint -> Effect Url
makeUrl server = makeUrlAddr (extractAddress server)

makeUrlWithPath
  :: forall r a. Newtype a { address :: ServerAddress | r }
  => a -> String -> Effect Url
makeUrlWithPath server = makeUrlAddrWithPath (extractAddress server)

makeUrlAddr :: ServerAddress -> Endpoint -> Effect Url
makeUrlAddr serverAddr ep =
  makeUrlAddrWithPath serverAddr (makePath ep)

makeUrlAddrWithPath :: ServerAddress -> String -> Effect Url
makeUrlAddrWithPath (ServerAddress host) path = do
  webC <- Config.webConfig
  pure $ wrap $ "http://" <> host <> ":" <> (show webC.supportPort) <> path

makeWsUrl
  :: forall r a. Newtype a { address :: ServerAddress | r }
  => a -> Endpoint -> Effect Url
makeWsUrl server ep = makeWsUrlAddr (extractAddress server) ep

makeWsUrlWithPath
  :: forall r a. Newtype a { address :: ServerAddress | r }
  => a -> String -> Effect Url
makeWsUrlWithPath server = makeWsUrlAddrWithPath (extractAddress server)

makeWsUrlAddr :: ServerAddress -> Endpoint -> Effect Url
makeWsUrlAddr serverAddr ep = do
  makeWsUrlAddrWithPath serverAddr (makePath ep)

makeWsUrlAddrWithPath :: ServerAddress -> String -> Effect Url
makeWsUrlAddrWithPath (ServerAddress host) path = do
  webC <- Config.webConfig
  pure $ wrap $ "ws://" <> host <> ":" <> (show webC.supportPort) <> path
