module Shared.Router.Endpoint ( Endpoint(..)
                              , Canary(..)
                              , endpoint
                              , makePath
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
import Routing.Duplex (RouteDuplex', as, path, print, rest, root, segment)
import Routing.Duplex.Generic (noArgs, sum)
import Routing.Duplex.Generic.Syntax ((/))
import Shared.Stream (ProfileName(..), RtmpShortName, SlotId, SlotIdAndProfileName(..), SlotNameAndProfileName(..), SlotRole(..))
import Shared.Types (PoPName, ServerAddress(..), Url, extractAddress)

data Canary = Live
            | Canary

data Endpoint
  = VMMetricsE
  | TransPoPLeaderE
  | IntraPoPTestHelperE
  | TimedRoutesE PoPName
  | HealthCheckE
  | ServerStateE
  | PoPDefinitionE
  | LoadE

  | EgestStatsE SlotId
  | EgestE

  | RelayE
  | RelayEnsureStartedE
  | RelayRegisterEgestE
  | RelayRegisterRelayE
  | RelayProxiedStatsE SlotId SlotRole
  | RelayStatsE SlotId SlotRole
  | RelaySlotConfigurationE SlotId SlotRole

  | IngestAggregatorE SlotId SlotRole
  | IngestAggregatorPlayerE SlotId SlotRole
  | IngestAggregatorPlayerJsE SlotId SlotRole (Array String)
  | IngestAggregatorActiveIngestsE SlotId SlotRole ProfileName
  | IngestAggregatorActiveIngestsPlayerE SlotId SlotRole ProfileName
  | IngestAggregatorActiveIngestsPlayerJsE SlotId SlotRole ProfileName (Array String)

  | IngestAggregatorActiveIngestsPlayerControlE SlotId SlotRole ProfileName

  | IngestAggregatorSlotConfigurationE SlotId SlotRole
  | IngestAggregatorRegisterRelayE
  | IngestAggregatorsE
  | IngestInstancesE
  | IngestInstancesMetricsE
  | IngestInstanceE SlotId ProfileName
  | IngestInstanceLlwpE SlotId SlotRole ProfileName
  | IngestStartE Canary RtmpShortName SlotNameAndProfileName
  | IngestStopE Canary SlotId SlotRole ProfileName

  | ClientAppAssetsE (Array String)
  | ClientAppRouteHTMLE

  | ClientStartE Canary SlotId
  | ClientStopE Canary SlotId String
  | ClientPlayerE Canary SlotId
  | ClientPlayerJsE Canary SlotId (Array String)
  | ClientPlayerControlE Canary SlotId

  | StreamAuthE
  | StreamAuthTypeE
  | StreamPublishE

  | WorkflowsE
  | WorkflowGraphE String
  | WorkflowMetricsE String
  | WorkflowStructureE String

derive instance genericEndpoint :: Generic Endpoint _

-- | Our codec will cause a compile-time error if we fail to handle any of our route cases.
endpoint :: RouteDuplex' Endpoint
endpoint = root $ sum
  {
    "VMMetricsE"                                       : "api" / "vm" / path "metrics" noArgs
  , "TransPoPLeaderE"                                  : "api" / path "transPoPLeader" noArgs
  , "IntraPoPTestHelperE"                              : "api" / "test" / path "intraPoP" noArgs
  , "TimedRoutesE"                                     : "api" / "timedRoutes" / popName segment
  , "HealthCheckE"                                     : "api" / path "healthCheck" noArgs
  , "ServerStateE"                                     : "api" / path "state" noArgs
  , "PoPDefinitionE"                                   : "api" / path "popDefinition" noArgs
  , "LoadE"                                            : "api" / path "load" noArgs

  , "EgestStatsE"                                      : "api" / "agents" / "egest" / slotId segment
  , "EgestE"                                           : "api" / "agents" / path "egest" noArgs

  , "RelayE"                                           : "api" / "agents" / "relay" / path "egest"  noArgs
  , "RelayEnsureStartedE"                              : "api" / "agents" / "relay" / path "ensureStarted"  noArgs
  , "RelayRegisterEgestE"                              : "api" / "agents" / "relay" / "register" / path "egest" noArgs
  , "RelayRegisterRelayE"                              : "api" / "agents" / "relay" / "register" / path "relay" noArgs
  , "RelayProxiedStatsE"                               : "api" / "agents" / "proxied" / "relay" / slotId segment / slotRole segment
  , "RelayStatsE"                                      : "api" / "agents" / "relay" / slotId segment / slotRole segment
  , "RelaySlotConfigurationE"                          : "api" / "agents" / "relay" / slotId segment / slotRole segment

  , "IngestAggregatorE"                                : "api" / "agents" / "ingestAggregator" / slotId segment / slotRole segment
  , "IngestAggregatorPlayerE"                          : "api" / "agents" / "ingestAggregator" / slotId segment / slotRole segment / "player"
  , "IngestAggregatorPlayerJsE"                        : "api" / "agents" / "ingestAggregator" / slotId segment / slotRole segment / "js" / rest
  , "IngestAggregatorActiveIngestsE"                   : "api" / "agents" / "ingestAggregator" / slotId segment / slotRole segment / "activeIngests" / profileName segment
  , "IngestAggregatorActiveIngestsPlayerE"             : "api" / "agents" / "ingestAggregator" / slotId segment / slotRole segment / "activeIngests" / profileName segment / "player"
  , "IngestAggregatorActiveIngestsPlayerJsE"           : "api" / "agents" / "ingestAggregator" / slotId segment / slotRole segment / "activeIngests" / profileName segment / "js" / rest
  , "IngestAggregatorActiveIngestsPlayerControlE"      : "api" / "agents" / "ingestAggregator" / slotId segment / slotRole segment / "activeIngests" / profileName segment / "control" -- URL duplicated in Web.purs
  , "IngestAggregatorSlotConfigurationE"               : "api" / "agents" / "ingestAggregator" / slotId segment / slotRole segment / "slot"
  , "IngestAggregatorRegisterRelayE"                   : "api" / "agents" / "ingestAggregator" / path "register" noArgs

  , "IngestAggregatorsE"                               : "api" / "agents" / path "ingestAggregator" noArgs

  , "IngestInstancesE"                                 : "api" / "agents" / path "ingest" noArgs
  , "IngestInstancesMetricsE"                          : "api" / "agents" / "ingest" / path "metrics" noArgs
  , "IngestInstanceE"                                  : "api" / "agents" / "ingest" / slotId segment / profileName segment
  , "IngestInstanceLlwpE"                              : "api" / "agents" / "ingest" / slotId segment / slotRole segment / profileName segment / "llwp" -- URL duplicated in Web.purs
  , "IngestStartE"                                     : "api" / "public" / canary segment / "ingest" / shortName segment / slotNameAndProfile segment / "start"
  , "IngestStopE"                                      : "api" / "public" / canary segment / "ingest" / slotId segment / slotRole segment / profileName segment / "stop"

  , "ClientAppAssetsE"                                 : "assets" / rest
  , "ClientAppRouteHTMLE"                              : "app" / noArgs

  , "ClientStartE"                                     : "api" / "public" / canary segment / "client" / slotId segment / "start"
  , "ClientStopE"                                      : "api" / "public" / canary segment / "client" / slotId segment / "stop" / segment
  , "ClientPlayerE"                                    : "api" / "public" / canary segment / "client" / slotId segment / "player"
  , "ClientPlayerJsE"                                  : "api" / "public" / canary segment / "client" / slotId segment / "js" / rest
  , "ClientPlayerControlE"                             : "api" / "public" / canary segment / "client" / slotId segment / "session" -- URL duplicated in Web.purs

  , "StreamAuthTypeE"                                  : "llnwstub" / "rts" / "v1" / path "streamauthtype" noArgs
  , "StreamAuthE"                                      : "llnwstub" / "rts" / "v1" / path "streamauth" noArgs
  , "StreamPublishE"                                   : "llnwstub" / "rts" / "v1" / path "streampublish" noArgs

  , "WorkflowsE"                                       : "api" / path "workflows" noArgs -- URL duplicated in Web.purs
  , "WorkflowGraphE"                                   : "api" / "workflows" / segment / "graph" -- URL duplicated in Web.purs
  , "WorkflowMetricsE"                                 : "api" / "workflows" / segment / "metrics" -- URL duplicated in Web.purs
  , "WorkflowStructureE"                               : "api" / "workflows" / segment / "structure" -- URL duplicated in Web.purs

}

makePath :: Endpoint -> String
makePath ep = print endpoint ep

makeUrl :: forall r a. Newtype a { address :: ServerAddress | r }
        => a -> Endpoint -> Url
makeUrl server ep = makeUrlAddr (extractAddress server) ep

makeUrlWithPath :: forall r a. Newtype a { address :: ServerAddress | r }
        => a -> String -> Url
makeUrlWithPath server path = makeUrlAddrWithPath (extractAddress server) path

makeUrlAddr :: ServerAddress -> Endpoint -> Url
makeUrlAddr serverAddr ep =
  makeUrlAddrWithPath serverAddr (makePath ep)

makeUrlAddrWithPath :: ServerAddress -> String -> Url
makeUrlAddrWithPath (ServerAddress host) path =
  wrap $ "http://" <> host <> ":3000" <> path

-- | SlotId
parseSlotId :: String -> Maybe SlotId
parseSlotId = ((<$>) wrap) <<< fromString

slotIdToString :: SlotId -> String
slotIdToString = show <<< unwrap

-- | ProfileName
parseProfileName :: String -> Maybe ProfileName
parseProfileName = wrapParser

profileNameToString :: ProfileName -> String
profileNameToString = unwrap

-- | SlotRole
parseSlotRole :: String -> Maybe SlotRole
parseSlotRole "primary" = Just Primary
parseSlotRole "backup" = Just Backup
parseSlotRole _ = Nothing

slotRoleToString :: SlotRole -> String
slotRoleToString Primary = "primary"
slotRoleToString Backup = "backup"

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
parseSlotIdAndProfileName ""  = Nothing
parseSlotIdAndProfileName str =
  case split (Pattern "_") str !! 0 of
    Just slotIdStr ->
      case fromString slotIdStr of
        Nothing -> Nothing
        Just slotIdVal -> Just (SlotIdAndProfileName (wrap slotIdVal) (wrap str))
    _ -> Nothing

parseSlotNameAndProfileName :: String -> Maybe SlotNameAndProfileName
parseSlotNameAndProfileName ""  = Nothing
parseSlotNameAndProfileName str =
  case split (Pattern "_") str of
    [slotNameStr, profileNameStr] -> Just (SlotNameAndProfileName slotNameStr (wrap profileNameStr))
    _ -> Nothing

slotNameAndProfileToString :: SlotNameAndProfileName -> String
slotNameAndProfileToString (SlotNameAndProfileName slotName (ProfileName str)) = slotName <> "_" <> str


-- | Canary
parseCanary :: String -> Maybe Canary
parseCanary "live"  = Just Live
parseCanary "canary"  = Just Canary
parseCanary _ = Nothing

canaryToString :: Canary -> String
canaryToString Live = "live"
canaryToString Canary = "canary"

-- | This combinator transforms a codec over `String` into one that operates on the `SlotId` type.
slotId :: RouteDuplex' String -> RouteDuplex' SlotId
slotId = as slotIdToString (parseSlotId >>> note "Bad SlotId")

-- | This combinator transforms a codec over `String` into one that operates on the `ProfileName` type.
profileName :: RouteDuplex' String -> RouteDuplex' ProfileName
profileName = as profileNameToString (parseProfileName >>> note "Bad ProfileName")

-- | This combinator transforms a codec over `String` into one that operates on the `ProfileName` type.
slotRole :: RouteDuplex' String -> RouteDuplex' SlotRole
slotRole = as slotRoleToString (parseSlotRole >>> note "Bad SlotRole")

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
canary = as canaryToString (parseCanary >>> note "Bad CanaryId")
