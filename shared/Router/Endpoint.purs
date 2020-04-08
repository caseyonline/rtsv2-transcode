module Shared.Router.Endpoint ( Endpoint(..)
                              , Canary(..)
                              , endpoint
                              , makePath
                              , makeUrl
                              , makeUrlWithPath
                              , makeUrlAddr
                              , makeUrlAddrWithPath
                              , makeWsUrl
                              , makeWsUrlWithPath
                              , makeWsUrlAddr
                              , makeWsUrlAddrWithPath
                              , parseSlotId
                              , parseSlotRole
                              , parseServerAddress
                              , parseSourceRoute
                              , parseInt
                              , uName
                              , popName
                              ) where

import Prelude hiding ((/))

import Data.Array (intercalate, (!!))
import Data.Either (note)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.String (Pattern(..), split)
import Routing.Duplex (RouteDuplex', as, path, print, rest, root, segment)
import Routing.Duplex.Generic (noArgs, sum)
import Routing.Duplex.Generic.Syntax ((/))
import Shared.Common (Url)
import Shared.Stream (ProfileName(..), RtmpShortName, SlotId, SlotIdAndProfileName(..), SlotNameAndProfileName(..), SlotRole(..))
import Shared.Types (JsonLdContextType(..), PoPName(..), ServerAddress(..), Username(..), SourceRoute, extractAddress)
import Shared.UUID (fromString)

data Canary = Live
            | Canary

instance showCanary :: Show Canary where
  show Live   = "live"
  show Canary = "canary"

data Endpoint
  =
  -- Public
    ClientPlayerE Canary SlotId SlotRole
  | ClientPlayerJsE Canary SlotId SlotRole (Array String)
  | ClientPlayerControlE Canary SlotId SlotRole

  -- Support
  | VMMetricsE
  | TimedRoutesE
  | TimedRoutesForPoPE PoPName
  | HealthCheckE
  | ServerStateE
  | SlotStateE SlotId
  | PoPDefinitionE
  | JsonLdContext JsonLdContextType
  | EgestStatsE SlotId SlotRole
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

  -- System
  | TransPoPLeaderE
  | EgestE
  | RelayE
  | RelayEnsureStartedE
  | RelayRegisteredRelayWs SlotId SlotRole ServerAddress Int SourceRoute
  | RelayRegisteredEgestWs SlotId SlotRole ServerAddress Int
  | IngestAggregatorRegisteredIngestWs SlotId SlotRole ProfileName ServerAddress
  | IngestAggregatorRegisteredRelayWs SlotId SlotRole ServerAddress Int
  | IngestAggregatorBackupWs SlotId SlotRole
  | IngestInstanceLlwpE SlotId SlotRole ProfileName
  | IntraPoPTestHelperE
  | LoadE
  | RelayProxiedStatsE SlotId SlotRole
  | Chaos

  | IngestStartE Canary RtmpShortName SlotNameAndProfileName
  | IngestStopE Canary SlotId SlotRole ProfileName

  | ClientStartE Canary SlotId SlotRole
  | ClientStopE Canary SlotId SlotRole String

  | StreamAuthTypeE
  | StreamAuthE
  | StreamPublishE

  | WorkflowsE
  | WorkflowGraphE String
  | WorkflowMetricsE String
  | WorkflowStructureE String

  -- | FrontEnd specific
  | LoginE
  | UserE
  | UsersE
  | ProfilesE Username

derive instance genericEndpoint :: Generic Endpoint _

instance showEndpoint :: Show Endpoint where
  show = genericShow

-- | Our codec will cause a compile-time error if we fail to handle any of our route cases.
endpoint :: RouteDuplex' Endpoint
endpoint = root $ sum
  {
  -- Public
    "ClientPlayerE"                                    : "public" / canary segment / "client" / slotId segment / slotRole segment / "player"
  , "ClientPlayerJsE"                                  : "public" / canary segment / "client" / slotId segment / slotRole segment / "js" / rest
  , "ClientPlayerControlE"                             : "public" / canary segment / "client" / slotId segment / slotRole segment / "session" -- URL duplicated in Web.purs

  -- Support
  , "VMMetricsE"                                       : "support" / "vm" / path "metrics" noArgs
  , "TimedRoutesE"                                     : "support" / path "timedRoutes" noArgs
  , "TimedRoutesForPoPE"                               : "support" / "timedRoutes" / popName segment
  , "HealthCheckE"                                     : "support" / path "healthCheck" noArgs
  , "ServerStateE"                                     : "support" / path "state" noArgs
  , "SlotStateE"                                       : "support" / "state" / "slot" / slotId segment
  , "PoPDefinitionE"                                   : "support" / path "popDefinition" noArgs
  , "JsonLdContext"                                    : "support" / "jsonld" / contextType segment
  , "EgestStatsE"                                      : "support" / "egest" / slotId segment / slotRole segment
  , "RelayStatsE"                                      : "support" / "relay" / slotId segment / slotRole segment  -- TODO - stats vs status
  , "IngestAggregatorE"                                : "support" / "ingestAggregator" / slotId segment / slotRole segment

  , "IngestAggregatorPlayerE"                          : "support" / "ingestAggregator" / slotId segment / slotRole segment / "player"
  , "IngestAggregatorPlayerJsE"                        : "support" / "ingestAggregator" / slotId segment / slotRole segment / "js" / rest
  , "IngestAggregatorActiveIngestsPlayerE"             : "support" / "ingestAggregator" / slotId segment / slotRole segment / "activeIngests" / profileName segment / "player"
  , "IngestAggregatorActiveIngestsPlayerJsE"           : "support" / "ingestAggregator" / slotId segment / slotRole segment / "activeIngests" / profileName segment / "js" / rest
  , "IngestAggregatorActiveIngestsPlayerControlE"      : "support" / "ingestAggregator" / slotId segment / slotRole segment / "activeIngests" / profileName segment / "control" -- URL duplicated in Web.purs
  , "IngestAggregatorsE"                               : "support" / path "ingestAggregator" noArgs
  , "IngestInstancesMetricsE"                          : "support" / "ingest" / path "metrics" noArgs
  , "IngestInstanceE"                                  : "support" / "ingest" / slotId segment / slotRole segment / profileName segment

  , "ClientAppAssetsE"                                 : "support" / "assets" / rest
  , "ClientAppRouteHTMLE"                              : "support" / noArgs

  -- System
  , "TransPoPLeaderE"                                  : "system" / path "transPoPLeader" noArgs

  , "EgestE"                                           : "system" / path "egest"  noArgs

  , "RelayE"                                           : "system" / "relay" / path "egest" noArgs
  , "RelayEnsureStartedE"                              : "system" / "relay" / path "ensureStarted" noArgs
  , "RelayRegisteredRelayWs"                           : "system" / "relay" / slotId segment / slotRole segment / "relays" / serverAddress segment / port segment / sourceRoute segment / "ws"
  , "RelayRegisteredEgestWs"                           : "system" / "relay" / slotId segment / slotRole segment / "egests" / serverAddress segment / port segment / "ws"

  , "IngestAggregatorRegisteredIngestWs"               : "system" / "ingestAggregator" / slotId segment / slotRole segment / "ingests" / profileName segment / serverAddress segment / "ws"
  , "IngestAggregatorRegisteredRelayWs"                : "system" / "ingestAggregator" / slotId segment / slotRole segment / "relays" / serverAddress segment / port segment / "ws"
  , "IngestAggregatorBackupWs"                         : "system" / "ingestAggregator" / slotId segment / slotRole segment / "backupws"

  , "IngestInstanceLlwpE"                              : "system" / "ingest" / slotId segment / slotRole segment / profileName segment / "llwp" -- URL duplicated in Web.purs

  , "IntraPoPTestHelperE"                              : "system" / "test" / path "intraPoP" noArgs
  , "LoadE"                                            : "system" / "test" / path "load" noArgs
  , "RelayProxiedStatsE"                               : "system" / "test" / "proxied" / "relay" / slotId segment / slotRole segment
  , "Chaos"                                            : "system" / "test" / path "chaos" noArgs

  , "IngestStartE"                                     : "system" / "test" / canary segment / "ingest" / shortName segment / slotNameAndProfile segment / "start"
  , "IngestStopE"                                      : "system" / "test" / canary segment / "ingest" / slotId segment / slotRole segment / profileName segment / "stop"
  , "ClientStartE"                                     : "system" / "test" / canary segment / "client" / slotId segment / slotRole segment / "start"
  , "ClientStopE"                                      : "system" / "test" / canary segment / "client" / slotId segment / slotRole segment / "stop" / segment

  , "StreamAuthTypeE"                                  : "system" / "llnwstub" / "rts" / "v1" / path "streamauthtype" noArgs
  , "StreamAuthE"                                      : "system" / "llnwstub" / "rts" / "v1" / path "streamauth" noArgs
  , "StreamPublishE"                                   : "system" / "llnwstub" / "rts" / "v1" / path "streampublish" noArgs

  , "WorkflowsE"                                       : "system" / path "workflows" noArgs -- URL duplicated in Web.purs
  , "WorkflowGraphE"                                   : "system" / "workflows" / segment / "graph" -- URL duplicated in Web.purs
  , "WorkflowMetricsE"                                 : "system" / "workflows" / segment / "metrics" -- URL duplicated in Web.purs
  , "WorkflowStructureE"                               : "system" / "workflows" / segment / "structure" -- URL duplicated in Web.purs

  -- Support UI URLs - TODO - to be deleted and moved to client
  , "LoginE"                                           : "api" / "users" / "login" / noArgs
  , "UserE"                                            : "api" / "user" / noArgs
  , "UsersE"                                           : "api" / "users" / noArgs
  , "ProfilesE"                                        : "api" / "profile" / uName segment

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


makeWsUrl :: forall r a. Newtype a { address :: ServerAddress | r }
        => a -> Endpoint -> Url
makeWsUrl server ep = makeWsUrlAddr (extractAddress server) ep

makeWsUrlWithPath :: forall r a. Newtype a { address :: ServerAddress | r }
        => a -> String -> Url
makeWsUrlWithPath server path = makeWsUrlAddrWithPath (extractAddress server) path

makeWsUrlAddr :: ServerAddress -> Endpoint -> Url
makeWsUrlAddr serverAddr ep =
  makeWsUrlAddrWithPath serverAddr (makePath ep)

makeWsUrlAddrWithPath :: ServerAddress -> String -> Url
makeWsUrlAddrWithPath (ServerAddress host) path =
  wrap $ "ws://" <> host <> ":3000" <> path

-- | JsonLd Context Type
contextTypeToString :: JsonLdContextType -> String
contextTypeToString ServerContext = "server"
contextTypeToString ServerAddressContext = "serverAddress"
contextTypeToString DeliverToContext = "deliverTo"
contextTypeToString TimedRouteNeighbourContext = "timedRouteNeighbour"
contextTypeToString ActiveIngestLocationContext = "activeIngestLocation"
contextTypeToString EgestStatsContext = "egestStats"
contextTypeToString IntraPoPStateContext = "intraPoPState"
contextTypeToString IngestAggregatorStateContext = "ingestAggregatorState"
contextTypeToString StreamRelayStateContext = "streamRelayState"
contextTypeToString IngestStateContext = "ingestState"

parseContextType :: String -> Maybe JsonLdContextType
parseContextType "server" = Just ServerContext
parseContextType "serverAddress" = Just ServerAddressContext
parseContextType "deliverTo" = Just DeliverToContext
parseContextType "timedRouteNeighbour" = Just TimedRouteNeighbourContext
parseContextType "activeIngestLocation" = Just ActiveIngestLocationContext
parseContextType "egestStats" = Just EgestStatsContext
parseContextType "intraPoPState" = Just IntraPoPStateContext
parseContextType "ingestAggregatorState" = Just IngestAggregatorStateContext
parseContextType "streamRelayState" = Just StreamRelayStateContext
parseContextType "ingestState" = Just IngestStateContext
parseContextType _ = Nothing

-- | Int
parseInt :: String -> Maybe Int
parseInt = Int.fromString

intToString :: Int -> String
intToString = show

-- | SourceRoute
parseSourceRoute :: String -> Maybe SourceRoute
parseSourceRoute str = Just $ PoPName <$> split (Pattern ":") str

sourceRouteToString :: SourceRoute -> String
sourceRouteToString route = intercalate ":" $ unwrap <$> route

-- | SlotId
parseSlotId :: String -> Maybe SlotId
parseSlotId = ((<$>) wrap) <<< fromString

slotIdToString :: SlotId -> String
slotIdToString = show <<< unwrap

-- | ServerAddress
parseServerAddress :: String -> Maybe ServerAddress
parseServerAddress = wrapParser

serverAddressToString :: ServerAddress -> String
serverAddressToString = unwrap

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


-- | Username
parseUsername :: String -> Maybe Username
parseUsername "" = Nothing
parseUsername str = Just (Username str)

userNametoString :: Username -> String
userNametoString (Username str) = str

-- | This combinator transforms a codec over `String` into one that operates on the `ContextType` type.
contextType :: RouteDuplex' String -> RouteDuplex' JsonLdContextType
contextType = as contextTypeToString (parseContextType >>> note "Bad ContextType")

-- | This combinator transforms a codec over `String` into one that operates on the `SlotId` type.
slotId :: RouteDuplex' String -> RouteDuplex' SlotId
slotId = as slotIdToString (parseSlotId >>> note "Bad SlotId")

-- | This combinator transforms a codec over `String` into one that operates on the `ProfileName` type.
profileName :: RouteDuplex' String -> RouteDuplex' ProfileName
profileName = as profileNameToString (parseProfileName >>> note "Bad ProfileName")

-- | This combinator transforms a codec over `String` into one that operates on the `ServerAddress` type.
serverAddress :: RouteDuplex' String -> RouteDuplex' ServerAddress
serverAddress = as serverAddressToString (parseServerAddress >>> note "Bad ServerAddress")

-- | This combinator transforms a codec over `String` into one that operates on the `ProfileName` type.
slotRole :: RouteDuplex' String -> RouteDuplex' SlotRole
slotRole = as slotRoleToString (parseSlotRole >>> note "Bad SlotRole")

-- | This combinator transforms a codec over `String` into one that operates on the `Int` type.
port :: RouteDuplex' String -> RouteDuplex' Int
port = as intToString (parseInt >>> note "Bad Port")

-- | This combinator transforms a codec over `String` into one that operates on the `Int` type.
sourceRoute :: RouteDuplex' String -> RouteDuplex' SourceRoute
sourceRoute = as sourceRouteToString (parseSourceRoute >>> note "Bad SourceRoute")

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

uName :: RouteDuplex' String -> RouteDuplex' Username
uName = as userNametoString (parseUsername >>> note "Bad username"
)
