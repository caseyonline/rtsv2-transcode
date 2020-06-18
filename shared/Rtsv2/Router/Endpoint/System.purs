module Shared.Rtsv2.Router.Endpoint.System where

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
import Shared.Rtsv2.Router.Endpoint.Combinators (shortName, slotName, slotId, slotRole, streamName, serverAddress, port, sourceRoute, profileName, canary)
import Shared.Rtsv2.Stream (ProfileName, RtmpShortName, RtmpStreamName, SlotId, SlotName, SlotRole)
import Shared.Rtsv2.Types (CanaryState, ServerAddress(..), SourceRoute, extractAddress)

data Endpoint
  =
  -- System
    TransPoPLeaderE
  | EgestE
  | RelayE
  | RelayEnsureStartedE
  | RelayRegisteredRelayWs SlotId SlotRole ServerAddress Int SourceRoute
  | RelayRegisteredEgestWs SlotId SlotRole ServerAddress Int

  | IngestAggregatorsE
  | IngestAggregatorRegisteredIngestWs SlotId SlotRole ProfileName ServerAddress
  | IngestAggregatorRegisteredRelayWs SlotId SlotRole ServerAddress Int
  | IngestAggregatorBackupWs SlotId SlotRole
  | IngestInstanceLlwpE SlotId SlotRole ProfileName
  | IntraPoPTestHelperE
  | LoadE
  | RelayProxiedStatsE SlotId SlotRole
  | Chaos

  | IngestStartE CanaryState RtmpShortName RtmpStreamName
  | IngestStopE SlotId SlotRole ProfileName

  | ClientStartE CanaryState SlotId SlotRole
  | ClientStopE SlotId SlotRole String

  | StreamAuthTypeE
  | StreamAuthE
  | StreamPublishE
  | SlotLookupE RtmpShortName SlotName
  | HlsPushE (Array String)
  | ValidationE String

  | WorkflowsE
  | WorkflowGraphE String
  | WorkflowMetricsE String
  | WorkflowStructureE String
  | WorkflowConfigE String
  | VisualiserE String
  | VisualiserAssetsE String (Array String)
  | FaviconE

derive instance genericEndpoint :: Generic Endpoint _

instance showEndpoint :: Show Endpoint where
  show = genericShow

-- | Our codec will cause a compile-time error if we fail to handle any of our route cases.
endpoint :: RouteDuplex' Endpoint
endpoint = root $ sum
  {
  -- System
    "TransPoPLeaderE"                                  : "system" / path "transPoPLeader" noArgs

  , "EgestE"                                           : "system" / path "egest"  noArgs

  , "RelayE"                                           : "system" / "relay" / path "egest" noArgs
  , "RelayEnsureStartedE"                              : "system" / "relay" / path "ensureStarted" noArgs
  , "RelayRegisteredRelayWs"                           : "system" / "relay" / slotId segment / slotRole segment / "relays" / serverAddress segment / port segment / sourceRoute segment / "ws"
  , "RelayRegisteredEgestWs"                           : "system" / "relay" / slotId segment / slotRole segment / "egests" / serverAddress segment / port segment / "ws"

  , "IngestAggregatorsE"                               : "system" / "ingestAggregator" / noArgs
  , "IngestAggregatorRegisteredIngestWs"               : "system" / "ingestAggregator" / slotId segment / slotRole segment / "ingests" / profileName segment / serverAddress segment / "ws"
  , "IngestAggregatorRegisteredRelayWs"                : "system" / "ingestAggregator" / slotId segment / slotRole segment / "relays" / serverAddress segment / port segment / "ws"
  , "IngestAggregatorBackupWs"                         : "system" / "ingestAggregator" / slotId segment / slotRole segment / "backupWs"

  , "IngestInstanceLlwpE"                              : "system" / "ingest" / slotId segment / slotRole segment / profileName segment / "llwp" -- URL duplicated in Web.purs

  , "IntraPoPTestHelperE"                              : "system" / "test" / path "intraPoP" noArgs

  , "RelayProxiedStatsE"                               : "system" / "test" / "proxied" / "relay" / slotId segment / slotRole segment

  , "Chaos"                                            : "system" / "test" / path "chaos" noArgs
  , "LoadE"                                            : "system" / "test" / path "load" noArgs
  , "IngestStartE"                                     : "system" / "test" / "ingest" / canary segment / shortName segment / streamName segment / "start"
  , "IngestStopE"                                      : "system" / "test" / "ingest" / slotId segment / slotRole segment / profileName segment / "stop"
  , "ClientStartE"                                     : "system" / "test" / "client" / canary segment / slotId segment / slotRole segment / "start"
  , "ClientStopE"                                      : "system" / "test" / "client" / slotId segment / slotRole segment / "stop" / segment

  , "SlotLookupE"                                      : "system" / "llnwstub" / "rts" / "v1" / "slotid" / shortName segment / slotName segment
  , "StreamAuthTypeE"                                  : "system" / "llnwstub" / "rts" / "v1" / path "streamauthtype" noArgs
  , "StreamAuthE"                                      : "system" / "llnwstub" / "rts" / "v1" / path "streamauth" noArgs
  , "StreamPublishE"                                   : "system" / "llnwstub" / "rts" / "v1" / path "streampublish" noArgs
  , "HlsPushE"                                         : "system" / "llnwstub" / "rts" / "v1" / "hls" / rest
  , "ValidationE"                                      : "system" / "llnwstub" / "rts" / "v1" / "validation" / segment

  , "WorkflowsE"                                       : "system" / path "workflows" noArgs -- URL duplicated in Web.purs
  , "WorkflowGraphE"                                   : "system" / "workflows" / segment / "graph" -- URL duplicated in Web.purs
  , "WorkflowMetricsE"                                 : "system" / "workflows" / segment / "metrics" -- URL duplicated in Web.purs
  , "WorkflowStructureE"                               : "system" / "workflows" / segment / "structure" -- URL duplicated in Web.purs
  , "WorkflowConfigE"                                  : "system" / "workflows" / segment / "workflow/config" -- URL duplicated in Web.purs
  , "VisualiserE"                                      : "system" / "workflows" / segment / "visualiser"
  , "VisualiserAssetsE"                                : "system" / "workflows" / segment / "visualiser" / rest
  , "FaviconE"                                         : "favicon.ico" / noArgs
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
  pure $ wrap $ "http://" <> host <> ":" <> (show webC.systemPort) <> path

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
  pure $ wrap $ "ws://" <> host <> ":" <> (show webC.systemPort) <> path

-- -- | JsonLd Context Type
-- contextTypeToString :: JsonLdContextType -> String
-- contextTypeToString ServerContext = "server"
-- contextTypeToString ServerAddressContext = "serverAddress"
-- contextTypeToString DeliverToContext = "deliverTo"
-- contextTypeToString TimedRouteNeighbourContext = "timedRouteNeighbour"
-- contextTypeToString ActiveIngestLocationContext = "activeIngestLocation"
-- contextTypeToString EgestStatsContext = "egestStats"
-- contextTypeToString IntraPoPStateContext = "intraPoPState"
-- contextTypeToString IngestAggregatorStateContext = "ingestAggregatorState"
-- contextTypeToString StreamRelayStateContext = "streamRelayState"
-- contextTypeToString IngestStateContext = "ingestState"
-- contextTypeToString NodeManagerStateContext = "nodeManagerState"
-- contextTypeToString HealthContext = "healthContext"

-- parseContextType :: String -> Maybe JsonLdContextType
-- parseContextType "server" = Just ServerContext
-- parseContextType "serverAddress" = Just ServerAddressContext
-- parseContextType "deliverTo" = Just DeliverToContext
-- parseContextType "timedRouteNeighbour" = Just TimedRouteNeighbourContext
-- parseContextType "activeIngestLocation" = Just ActiveIngestLocationContext
-- parseContextType "egestStats" = Just EgestStatsContext
-- parseContextType "intraPoPState" = Just IntraPoPStateContext
-- parseContextType "ingestAggregatorState" = Just IngestAggregatorStateContext
-- parseContextType "streamRelayState" = Just StreamRelayStateContext
-- parseContextType "ingestState" = Just IngestStateContext
-- parseContextType "nodeManagerState" = Just NodeManagerStateContext
-- parseContextType "healthContext" = Just HealthContext
-- parseContextType _ = Nothing

-- -- | Int
-- parseInt :: String -> Maybe Int
-- parseInt = Int.fromString

-- intToString :: Int -> String
-- intToString = show

-- -- | SourceRoute
-- parseSourceRoute :: String -> Maybe SourceRoute
-- parseSourceRoute str = Just $ PoPName <$> split (Pattern ":") str

-- sourceRouteToString :: SourceRoute -> String
-- sourceRouteToString route = intercalate ":" $ unwrap <$> route

-- -- | SlotId
-- parseSlotId :: String -> Maybe SlotId
-- parseSlotId = ((<$>) wrap) <<< fromString

-- slotIdToString :: SlotId -> String
-- slotIdToString = show <<< unwrap

-- -- | ServerAddress
-- parseServerAddress :: String -> Maybe ServerAddress
-- parseServerAddress = wrapParser

-- serverAddressToString :: ServerAddress -> String
-- serverAddressToString = unwrap

-- -- | ProfileName
-- parseProfileName :: String -> Maybe ProfileName
-- parseProfileName = wrapParser

-- profileNameToString :: ProfileName -> String
-- profileNameToString = unwrap

-- -- | SlotRole
-- parseSlotRole :: String -> Maybe SlotRole
-- parseSlotRole "primary" = Just Primary
-- parseSlotRole "backup" = Just Backup
-- parseSlotRole _ = Nothing

-- slotRoleToString :: SlotRole -> String
-- slotRoleToString Primary = "primary"
-- slotRoleToString Backup = "backup"

-- -- | RtmpShortName
-- parseRtmpShortName :: String -> Maybe RtmpShortName
-- parseRtmpShortName = wrapParser

-- shortNameToString :: RtmpShortName -> String
-- shortNameToString = unwrap


-- -- | RtmpStreamName
-- parseRtmpStreamName :: String -> Maybe RtmpStreamName
-- parseRtmpStreamName = wrapParser

-- streamNameToString :: RtmpStreamName -> String
-- streamNameToString = unwrap


-- -- | Generic parser for newtypes
-- wrapParser :: forall a. Newtype a String => String -> Maybe a
-- wrapParser "" = Nothing
-- wrapParser str = Just $ wrap str

-- -- | PoPName
-- parsePoPName :: String -> Maybe PoPName
-- parsePoPName  = wrapParser

-- poPNameToString :: PoPName -> String
-- poPNameToString = unwrap

-- -- | Username
-- parseUsername :: String -> Maybe Username
-- parseUsername "" = Nothing
-- parseUsername str = Just (Username str)

-- userNametoString :: Username -> String
-- userNametoString (Username str) = str

-- -- | CanaryState
-- parseCanaryState :: String -> Maybe CanaryState
-- parseCanaryState "live"  = Just Live
-- parseCanaryState "canary"  = Just Canary
-- parseCanaryState _ = Nothing

-- canaryStateToString :: CanaryState -> String
-- canaryStateToString Live = "live"
-- canaryStateToString Canary = "canary"

-- -- | This combinator transforms a codec over `String` into one that operates on the `ContextType` type.
-- contextType :: RouteDuplex' String -> RouteDuplex' JsonLdContextType
-- contextType = as contextTypeToString (parseContextType >>> note "Bad ContextType")

-- -- | This combinator transforms a codec over `String` into one that operates on the `SlotId` type.
-- slotId :: RouteDuplex' String -> RouteDuplex' SlotId
-- slotId = as slotIdToString (parseSlotId >>> note "Bad SlotId")

-- -- | This combinator transforms a codec over `String` into one that operates on the `ProfileName` type.
-- profileName :: RouteDuplex' String -> RouteDuplex' ProfileName
-- profileName = as profileNameToString (parseProfileName >>> note "Bad ProfileName")

-- -- | This combinator transforms a codec over `String` into one that operates on the `ServerAddress` type.
-- serverAddress :: RouteDuplex' String -> RouteDuplex' ServerAddress
-- serverAddress = as serverAddressToString (parseServerAddress >>> note "Bad ServerAddress")

-- -- | This combinator transforms a codec over `String` into one that operates on the `ProfileName` type.
-- slotRole :: RouteDuplex' String -> RouteDuplex' SlotRole
-- slotRole = as slotRoleToString (parseSlotRole >>> note "Bad SlotRole")

-- -- | This combinator transforms a codec over `String` into one that operates on the `Int` type.
-- port :: RouteDuplex' String -> RouteDuplex' Int
-- port = as intToString (parseInt >>> note "Bad Port")

-- -- | This combinator transforms a codec over `String` into one that operates on the `Int` type.
-- sourceRoute :: RouteDuplex' String -> RouteDuplex' SourceRoute
-- sourceRoute = as sourceRouteToString (parseSourceRoute >>> note "Bad SourceRoute")

-- -- | This combinator transforms a codec over `String` into one that operates on the `PoPName` type.
-- popName :: RouteDuplex' String -> RouteDuplex' PoPName
-- popName = as poPNameToString (parsePoPName >>> note "Bad PoPName")

-- -- | This combinator transforms a codec over `String` into one that operates on the `RtmpStreamName` type.
-- streamName :: RouteDuplex' String -> RouteDuplex' RtmpStreamName
-- streamName = as streamNameToString (parseRtmpStreamName >>> note "Bad RtmpStreamName")

-- -- | This combinator transforms a codec over `String` into one that operates on the `RtmpShortName` type.
-- shortName :: RouteDuplex' String -> RouteDuplex' RtmpShortName
-- shortName = as shortNameToString (parseRtmpShortName >>> note "Bad RtmpShortName")

-- -- | This combinator transforms a codec over `String` into one that operates on the `CanaryState` type.
-- canaryState :: RouteDuplex' String -> RouteDuplex' CanaryState
-- canaryState = as canaryStateToString (parseCanaryState >>> note "Bad CanaryId")

-- uName :: RouteDuplex' String -> RouteDuplex' Username
-- uName = as userNametoString (parseUsername >>> note "Bad username")
