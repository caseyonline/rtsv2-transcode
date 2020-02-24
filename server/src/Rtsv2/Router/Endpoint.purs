module Rtsv2.Router.Endpoint ( Endpoint(..)
                             , Canary(..)
                             , endpoint
                             , makeUrl
                             , makeUrlWithPath
                             , makeUrlAddr
                             , makeUrlAddrWithPath
                             , parseSlotRole
                             ) where

import Prelude hiding ((/))

import Data.Array ((!!))
import Data.Either (note)
import Data.Generic.Rep (class Generic)
import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.String (Pattern(..), split)
import Routing.Duplex (RouteDuplex', as, path, root, segment)
import Routing.Duplex.Generic (noArgs, sum)
import Routing.Duplex.Generic.Syntax ((/))
import Rtsv2.Router.Parser as Routing
import Shared.Stream (ProfileName(..), RtmpShortName, SlotId, SlotIdAndProfileName(..), SlotNameAndProfileName(..), SlotRole(..))
import Shared.Types (PoPName, ServerAddress(..), extractAddress)
import SpudGun (Url)

-- data Canary = Live
--             | Canary

type Canary = String

data Endpoint
  = VMMetricsE
  | TransPoPLeaderE
  | IntraPoPTestHelperE
  | TimedRoutesE PoPName
  | HealthCheckE
  | ServerStateE
  | PoPDefinitionE
  | EgestStatsE SlotId
  | EgestE
  | RelayE
  | RelayEnsureStartedE
  | RelayRegisterEgestE
  | RelayRegisterRelayE
  | RelayProxiedStatsE SlotId SlotRole
  | RelayStatsE SlotId SlotRole
  | LoadE
  | WorkflowsE
  | WorkflowGraphE String
  | WorkflowMetricsE String
  | WorkflowStructureE String
  | IngestAggregatorE SlotId SlotRole
  | IngestAggregatorPlayerE SlotId SlotRole
  | IngestAggregatorPlayerJsE SlotId SlotRole
  | IngestAggregatorActiveIngestsE SlotId SlotRole ProfileName
  | IngestAggregatorActiveIngestsPlayerE SlotId SlotRole ProfileName
  | IngestAggregatorActiveIngestsPlayerJsE SlotId SlotRole ProfileName
  | IngestAggregatorActiveIngestsPlayerControlE SlotId SlotRole ProfileName
  | IngestAggregatorRegisterRelayE
  | IngestAggregatorsE
  | IngestInstancesE
  | IngestInstancesMetricsE
  | IngestInstanceE SlotId ProfileName
  | IngestInstanceLlwpE SlotId SlotRole ProfileName
  | IngestStartE Canary RtmpShortName SlotNameAndProfileName
  | IngestStopE Canary SlotId SlotRole ProfileName
  | ClientAppAssetsE
  | ClientAppRouteHTMLE
  | ClientStartE Canary SlotId
  | ClientStopE Canary SlotId
  | StreamAuthE
  | StreamAuthTypeE
  | StreamPublishE

derive instance genericEndpoint :: Generic Endpoint _

-- | Our codec will cause a compile-time error if we fail to handle any of our route cases.
endpoint :: RouteDuplex' Endpoint
endpoint = root $ sum
  {
    "VMMetricsE"                                       : "" / "api" / "vm" / path "metrics" noArgs
  , "TransPoPLeaderE"                                  : "" / "api" / path "transPoPLeader" noArgs
  , "IntraPoPTestHelperE"                              : "" / "api" / "test" / path "intraPoP" noArgs
  , "TimedRoutesE"                                     : "" / "api" / "timedRoutes" / popName segment
  , "HealthCheckE"                                     : "" / "api" / path "healthCheck" noArgs
  , "ServerStateE"                                     : "" / "api" / path "state" noArgs
  , "PoPDefinitionE"                                   : "" / "api" / path "popDefinition" noArgs
  , "EgestStatsE"                                      : "" / "api" / "agents" / "egest" / streamId segment
  , "EgestE"                                           : "" / "api" / "agents" / path "egest" noArgs

  , "RelayE"                                           : "" / "api" / "agents" / "relay" / path "egest"  noArgs
  , "RelayEnsureStartedE"                              : "" / "api" / "agents" / "relay" / path "ensureStarted"  noArgs
  , "RelayRegisterEgestE"                              : "" / "api" / "agents" / "relay" / "register" / path "egest" noArgs
  , "RelayRegisterRelayE"                              : "" / "api" / "agents" / "relay" / "register" / path "relay" noArgs
  , "RelayStatsE"                                      : "" / "api" / "agents" / "relay" / streamId segment / streamRole segment
  , "RelayProxiedStatsE"                               : "" / "api" / "agents" / "proxied" / "relay" / streamId segment / streamRole segment

  , "LoadE"                                            : "" / "api" / path "load" noArgs

  , "WorkflowsE"                                       : "" / "api" / path "workflows" noArgs
  , "WorkflowGraphE"                                   : "" / "api" / "workflows" / segment / "graph"
  , "WorkflowMetricsE"                                 : "" / "api" / "workflows" / segment / "metrics"
  , "WorkflowStructureE"                               : "" / "api" / "workflows" / segment / "structure"

  , "IngestAggregatorE"                                : "" / "api" / "agents" / "ingestAggregator" / streamId segment / streamRole segment
  , "IngestAggregatorPlayerE"                          : "" / "api" / "agents" / "ingestAggregator" / streamId segment / streamRole segment / "player"
  , "IngestAggregatorPlayerJsE"                        : "" / "api" / "agents" / "ingestAggregator" / streamId segment / streamRole segment / "js" -- TODO - would like to add '/ "[...]"' bit it causes compiler error that I don't understand
  , "IngestAggregatorActiveIngestsE"                   : "" / "api" / "agents" / "ingestAggregator" / streamId segment / streamRole segment / "activeIngests" / variant segment
  , "IngestAggregatorActiveIngestsPlayerE"             : "" / "api" / "agents" / "ingestAggregator" / streamId segment / streamRole segment / "activeIngests" / variant segment / "player" -- TODO - streamRole for these as well
  , "IngestAggregatorActiveIngestsPlayerJsE"           : "" / "api" / "agents" / "ingestAggregator" / streamId segment / streamRole segment / "activeIngests" / variant segment / "js" -- TODO - would like to add '/ "[...]"' bit it causes compiler error that I don't understand
  , "IngestAggregatorActiveIngestsPlayerControlE"      : "" / "api" / "agents" / "ingestAggregator" / streamId segment / streamRole segment / "activeIngests" / variant segment / "control"

  , "IngestAggregatorRegisterRelayE"                   : "" / "api" / "agents" / "ingestAggregator" / path "register" noArgs
  , "IngestAggregatorsE"                               : "" / "api" / "agents" / path "ingestAggregator" noArgs

  , "IngestInstancesE"                                 : "" / "api" / "agents" / path "ingest" noArgs
  , "IngestInstancesMetricsE"                          : "" / "api" / "agents" / "ingest" / path "metrics" noArgs
  , "IngestInstanceE"                                  : "" / "api" / "agents" / "ingest" / streamId segment / variant segment
  , "IngestInstanceLlwpE"                              : "" / "api" / "agents" / "ingest" / streamId segment / streamRole segment / variant segment / "llwp"

  , "IngestStartE"                                     : "" / "api" / "public" / canary segment / "ingest" / shortName segment / slotNameAndProfile segment / "start"
  , "IngestStopE"                                      : "" / "api" / "public" / canary segment / "ingest" / streamId segment / streamRole segment / variant  segment / "stop"
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

makeUrlWithPath :: forall r a. Newtype a { address :: ServerAddress | r }
        => a -> String -> Url
makeUrlWithPath server path = makeUrlAddrWithPath (extractAddress server) path

makeUrlAddr :: ServerAddress -> Endpoint -> Url
makeUrlAddr serverAddr ep =
  let
    url = Routing.printUrl endpoint ep
  in
    makeUrlAddrWithPath serverAddr url

makeUrlAddrWithPath :: ServerAddress -> String -> Url
makeUrlAddrWithPath (ServerAddress host) path =
  wrap $ "http://" <> host <> ":3000" <> path


-- | SlotId
parseSlotId :: String -> Maybe SlotId
parseSlotId = ((<$>) wrap) <<< fromString

streamIdToString :: SlotId -> String
streamIdToString = show <<< unwrap

-- | ProfileName
parseProfileName :: String -> Maybe ProfileName
parseProfileName = wrapParser

variantToString :: ProfileName -> String
variantToString = unwrap

-- | SlotRole
parseSlotRole :: String -> Maybe SlotRole
parseSlotRole "primary" = Just Primary
parseSlotRole "backup" = Just Backup
parseSlotRole _ = Nothing

streamRoleToString :: SlotRole -> String
streamRoleToString Primary = "primary"
streamRoleToString Backup = "backup"

-- | RtmpShortName
parseRtmpShortName :: String -> Maybe RtmpShortName
parseRtmpShortName = wrapParser

shortNameToString :: RtmpShortName -> String
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


-- | SlotIdAndProfileName
parseSlotIdAndProfileName :: String -> Maybe SlotIdAndProfileName
parseSlotIdAndProfileName  ""  = Nothing
parseSlotIdAndProfileName  str =
  case split (Pattern "_") str !! 0 of
    Just streamIdStr ->
      case fromString streamIdStr of
        Nothing -> Nothing
        Just slotId -> Just (SlotIdAndProfileName (wrap slotId) (wrap str))
    _ -> Nothing

parseSlotNameAndProfileName :: String -> Maybe SlotNameAndProfileName
parseSlotNameAndProfileName  ""  = Nothing
parseSlotNameAndProfileName  str =
  case split (Pattern "_") str !! 0 of
    Just streamNameStr -> Just (SlotNameAndProfileName streamNameStr (wrap str))
    _ -> Nothing

slotNameAndProfileToString :: SlotNameAndProfileName -> String
slotNameAndProfileToString (SlotNameAndProfileName _ (ProfileName str)) = str


-- | Canary
-- parseCanary :: String -> Maybe Canary
-- parseCanary "live"  = Just Live
-- parseCanary "canary"  = Just Canary
-- parseCanary _ = Nothing

-- canaryToString :: Canary -> String
-- canaryToString Live = "live"
-- canaryToString Canary = "canary"

-- | This combinator transforms a codec over `String` into one that operates on the `SlotId` type.
streamId :: RouteDuplex' String -> RouteDuplex' SlotId
streamId = as streamIdToString (parseSlotId >>> note "Bad SlotId")

-- | This combinator transforms a codec over `String` into one that operates on the `ProfileName` type.
variant :: RouteDuplex' String -> RouteDuplex' ProfileName
variant = as variantToString (parseProfileName >>> note "Bad SlotId")

-- | This combinator transforms a codec over `String` into one that operates on the `ProfileName` type.
streamRole :: RouteDuplex' String -> RouteDuplex' SlotRole
streamRole = as streamRoleToString (parseSlotRole >>> note "Bad SlotRole")

-- | This combinator transforms a codec over `String` into one that operates on the `SlotNameAndProfileName` type.
slotNameAndProfile :: RouteDuplex' String -> RouteDuplex' SlotNameAndProfileName
slotNameAndProfile = as slotNameAndProfileToString (parseSlotNameAndProfileName >>> note "Bad SlotNameAndProfileName")

-- | This combinator transforms a codec over `String` into one that operates on the `PoPName` type.
popName :: RouteDuplex' String -> RouteDuplex' PoPName
popName = as poPNameToString (parsePoPName >>> note "Bad PoPName")

-- | This combinator transforms a codec over `String` into one that operates on the `RtmpShortName` type.
shortName :: RouteDuplex' String -> RouteDuplex' RtmpShortName
shortName = as shortNameToString (parseRtmpShortName >>> note "Bad RtmpShortName")

-- | This combinator transforms a codec over `String` into one that operates on the `Canary` type.
canary :: RouteDuplex' String -> RouteDuplex' Canary
--canary = as canaryToString (parseCanary >>> note "Bad CanaryId")
canary = as identity (Just >>> note "Bad CanaryId")
