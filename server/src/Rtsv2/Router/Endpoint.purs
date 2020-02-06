module Rtsv2.Router.Endpoint ( Endpoint(..)
                             , Canary(..)
                             , endpoint
                             , makeUrl
                             , makeUrlAddr
                             ) where

import Prelude hiding ((/))

import Data.Array ((!!))
import Data.Either (note)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.String (Pattern(..), split)
import Routing.Duplex (RouteDuplex', as, path, root, segment)
import Routing.Duplex.Generic (noArgs, sum)
import Routing.Duplex.Generic.Syntax ((/))
import Rtsv2.Router.Parser as Routing
import Shared.Stream (ShortName, StreamAndVariant(..), StreamId, StreamVariant(..))
import Shared.Types (PoPName, ServerAddress, extractAddress)
import SpudGun (Url)

-- data Canary = Live
--             | Canary

type Canary = String

data Endpoint
  = TransPoPLeaderE
  | TimedRoutesE PoPName
  | HealthCheckE
  | ServerStateE
  | PoPDefinitionE
  | EgestStatsE StreamId
  | EgestE
  | RelayE
  | RelayEnsureStartedE
  | RelayChainE
  | RelayRegisterE
  | RelayProxiedStatsE StreamId
  | RelayStatsE StreamId
  | LoadE
  | WorkflowsE
  | WorkflowGraphE String
  | WorkflowMetricsE String
  | WorkflowStructureE String
  | IngestAggregatorE StreamId
  | IngestAggregatorPlayerE StreamId
  | IngestAggregatorPlayerJsE StreamId
  | IngestAggregatorActiveIngestsE StreamId StreamVariant
  | IngestAggregatorActiveIngestsPlayerE StreamId StreamVariant
  | IngestAggregatorActiveIngestsPlayerJsE StreamId StreamVariant
  | IngestAggregatorActiveIngestsPlayerSessionStartE StreamId StreamVariant
  | IngestAggregatorActiveIngestsPlayerSessionE StreamId StreamVariant String
  | IngestAggregatorsE
  | IngestInstancesE
  | IngestInstancesStatsE
  | IngestInstanceE StreamId StreamVariant
  | IngestInstanceLlwpE StreamId StreamVariant
  | IngestStartE Canary ShortName StreamAndVariant
  | IngestStopE Canary ShortName StreamAndVariant
  | ClientAppAssetsE
  | ClientAppRouteHTMLE
  | ClientStartE Canary StreamId
  | ClientStopE Canary StreamId
  | StreamAuthE
  | StreamAuthTypeE
  | StreamPublishE

derive instance genericEndpoint :: Generic Endpoint _

-- | Our codec will cause a compile-time error if we fail to handle any of our route cases.
endpoint :: RouteDuplex' Endpoint
endpoint = root $ sum
  {
    "TransPoPLeaderE"                                  : "" / "api" / path "transPoPLeader" noArgs
  , "TimedRoutesE"                                     : "" / "api" / "timedRoutes" / popName segment
  , "HealthCheckE"                                     : "" / "api" / path "healthCheck" noArgs
  , "ServerStateE"                                     : "" / "api" / path "state" noArgs
  , "PoPDefinitionE"                                   : "" / "api" / path "popDefinition" noArgs
  , "EgestStatsE"                                      : "" / "api" / "agents" / "egest" / streamId segment
  , "EgestE"                                           : "" / "api" / "agents" / path "egest" noArgs

  , "RelayE"                                           : "" / "api" / "agents" / "relay" / path "egest"  noArgs
  , "RelayEnsureStartedE"                              : "" / "api" / "agents" / "relay" / path "ensureStarted"  noArgs
  , "RelayChainE"                                      : "" / "api" / "agents" / "relay" / path "chain"  noArgs
  , "RelayRegisterE"                                   : "" / "api" / "agents" / "relay" / path "register" noArgs
  , "RelayStatsE"                                      : "" / "api" / "agents" / "relay" / streamId segment
  , "RelayProxiedStatsE"                               : "" / "api" / "agents" / "proxied" / "relay" / streamId segment

  , "LoadE"                                            : "" / "api" / path "load" noArgs

  , "WorkflowsE"                                       : "" / "api" / path "workflows" noArgs
  , "WorkflowGraphE"                                   : "" / "api" / "workflows" / segment / "graph"
  , "WorkflowMetricsE"                                 : "" / "api" / "workflows" / segment / "metrics"
  , "WorkflowStructureE"                               : "" / "api" / "workflows" / segment / "structure"

  , "IngestAggregatorE"                                : "" / "api" / "agents" / "ingestAggregator" / streamId segment
  , "IngestAggregatorPlayerE"                          : "" / "api" / "agents" / "ingestAggregator" / streamId segment / "player"
  , "IngestAggregatorPlayerJsE"                        : "" / "api" / "agents" / "ingestAggregator" / streamId segment / "js" -- TODO - would like to add '/ "[...]"' bit it causes compiler error that I don't understand
  , "IngestAggregatorActiveIngestsE"                   : "" / "api" / "agents" / "ingestAggregator" / streamId segment / "activeIngests" / variant segment
  , "IngestAggregatorActiveIngestsPlayerE"             : "" / "api" / "agents" / "ingestAggregator" / streamId segment / "activeIngests" / variant segment / "player"
  , "IngestAggregatorActiveIngestsPlayerJsE"           : "" / "api" / "agents" / "ingestAggregator" / streamId segment / "activeIngests" / variant segment / "js" -- TODO - would like to add '/ "[...]"' bit it causes compiler error that I don't understand
  , "IngestAggregatorActiveIngestsPlayerSessionStartE" : "" / "api" / "agents" / "ingestAggregator" / streamId segment / "activeIngests" / variant segment / "session"
  , "IngestAggregatorActiveIngestsPlayerSessionE"      : "" / "api" / "agents" / "ingestAggregator" / streamId segment / "activeIngests" / variant segment / "session" / segment

  , "IngestAggregatorsE"                               : "" / "api" / "agents" / path "ingestAggregator" noArgs

  , "IngestInstancesE"                                 : "" / "api" / "agents" / path "ingest" noArgs
  , "IngestInstancesStatsE"                            : "" / "api" / "agents" / "ingest" / path "stats" noArgs
  , "IngestInstanceE"                                  : "" / "api" / "agents" / "ingest" / streamId segment / variant segment
  , "IngestInstanceLlwpE"                              : "" / "api" / "agents" / "ingest" / streamId segment / variant segment / "llwp"

  , "IngestStartE"                                     : "" / "api" / "public" / canary segment / "ingest" / shortName segment / streamAndVariant segment / "start"
  , "IngestStopE"                                      : "" / "api" / "public" / canary segment / "ingest" / shortName segment / streamAndVariant segment / "stop"
  , "ClientStartE"                                     : "" / "api" / "public" / canary segment / "client" / streamId segment / "start"
  , "ClientStopE"                                      : "" / "api" / "public" / canary segment / "client" / streamId segment / "stop"

  , "ClientAppAssetsE"                                 : "" / "app" / path "assets" noArgs
  , "ClientAppRouteHTMLE"                              : "" / "app" / noArgs

  , "StreamAuthE"                                      : "" / "llnwstub" / "rts" / "v1" / path "streamauthtype" noArgs
  , "StreamAuthTypeE"                                  : "" / "llnwstub" / "rts" / "v1" / path "streamauth" noArgs
  , "StreamPublishE"                                   : "" / "llnwstub" / "rts" / "v1" / path "streampublish" noArgs
  }


makeUrl :: forall r a. Newtype a { address :: ServerAddress | r }
        => a -> Endpoint -> Url
makeUrl server ep = makeUrlAddr (extractAddress server) ep

makeUrlAddr :: ServerAddress -> Endpoint -> Url
makeUrlAddr serverAddr ep =
  let
    path = Routing.printUrl endpoint ep
  in wrap $ "http://" <> toHost serverAddr <> ":3000" <> path
  where
    toHost = unwrap


-- | StreamId

parseStreamId :: String -> Maybe StreamId
parseStreamId = wrapParser

streamIdToString :: StreamId -> String
streamIdToString = unwrap

-- | StreamVariant
parseStreamVariant :: String -> Maybe StreamVariant
parseStreamVariant = wrapParser

variantToString :: StreamVariant -> String
variantToString = unwrap

-- | ShortName
parseShortName :: String -> Maybe ShortName
parseShortName = wrapParser

shortNameToString :: ShortName -> String
shortNameToString = unwrap


-- | Generic parser for newtypes
wrapParser :: forall a. Newtype a String => String -> Maybe a
wrapParser "" = Nothing
wrapParser str = Just $ wrap str


-- | PoPName
parsePoPName :: String -> Maybe PoPName
parsePoPName  = wrapParser

poPNameToString :: PoPName -> String
poPNameToString = unwrap


-- | StreamAndVariant
parseStreamAndVariant :: String -> Maybe StreamAndVariant
parseStreamAndVariant  ""  = Nothing
parseStreamAndVariant  str =
  case split (Pattern "_") str !! 0 of
    Just streamIdStr -> Just (StreamAndVariant (wrap streamIdStr) (wrap str))
    _ -> Nothing

streamAndVariantToString :: StreamAndVariant -> String
streamAndVariantToString (StreamAndVariant _ (StreamVariant str)) = str



-- | Canary
-- parseCanary :: String -> Maybe Canary
-- parseCanary "live"  = Just Live
-- parseCanary "canary"  = Just Canary
-- parseCanary _ = Nothing

-- canaryToString :: Canary -> String
-- canaryToString Live = "live"
-- canaryToString Canary = "canary"

-- | This combinator transforms a codec over `String` into one that operates on the `StreamId` type.
streamId :: RouteDuplex' String -> RouteDuplex' StreamId
streamId = as streamIdToString (parseStreamId >>> note "Bad StreamId")

-- | This combinator transforms a codec over `String` into one that operates on the `StreamVariant` type.
variant :: RouteDuplex' String -> RouteDuplex' StreamVariant
variant = as variantToString (parseStreamVariant >>> note "Bad StreamId")

-- | This combinator transforms a codec over `String` into one that operates on the `StreamAndVariant` type.
streamAndVariant :: RouteDuplex' String -> RouteDuplex' StreamAndVariant
streamAndVariant = as streamAndVariantToString (parseStreamAndVariant >>> note "Bad StreamAndVariant")

-- | This combinator transforms a codec over `String` into one that operates on the `PoPName` type.
popName :: RouteDuplex' String -> RouteDuplex' PoPName
popName = as poPNameToString (parsePoPName >>> note "Bad PoPName")

-- | This combinator transforms a codec over `String` into one that operates on the `ShortName` type.
shortName :: RouteDuplex' String -> RouteDuplex' ShortName
shortName = as shortNameToString (parseShortName >>> note "Bad ShortName")

-- | This combinator transforms a codec over `String` into one that operates on the `Canary` type.
canary :: RouteDuplex' String -> RouteDuplex' Canary
--canary = as canaryToString (parseCanary >>> note "Bad CanaryId")
canary = as identity (Just >>> note "Bad CanaryId")
