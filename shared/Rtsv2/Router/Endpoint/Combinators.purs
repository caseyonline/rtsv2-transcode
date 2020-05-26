module Shared.Rtsv2.Router.Endpoint.Combinators where

import Prelude hiding ((/))

import Data.Array (intercalate)
import Data.Either (note)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.String (Pattern(..), split)
import Routing.Duplex (RouteDuplex', as)
import Shared.Rtsv2.Stream (ProfileName, RtmpShortName, RtmpStreamName, SlotId, SlotRole(..))
import Shared.Rtsv2.Types (CanaryState(..), JsonLdContextType(..), PoPName(..), ServerAddress, SourceRoute, Username(..))
import Shared.UUID (fromString)


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
contextTypeToString NodeManagerStateContext = "nodeManagerState"
contextTypeToString HealthContext = "healthContext"

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
parseContextType "nodeManagerState" = Just NodeManagerStateContext
parseContextType "healthContext" = Just HealthContext
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


-- | RtmpStreamName
parseRtmpStreamName :: String -> Maybe RtmpStreamName
parseRtmpStreamName = wrapParser

streamNameToString :: RtmpStreamName -> String
streamNameToString = unwrap


-- | Generic parser for newtypes
wrapParser :: forall a. Newtype a String => String -> Maybe a
wrapParser "" = Nothing
wrapParser str = Just $ wrap str

-- | PoPName
parsePoPName :: String -> Maybe PoPName
parsePoPName  = wrapParser

poPNameToString :: PoPName -> String
poPNameToString = unwrap

-- | Username
parseUsername :: String -> Maybe Username
parseUsername "" = Nothing
parseUsername str = Just (Username str)

userNametoString :: Username -> String
userNametoString (Username str) = str

-- | CanaryState
parseCanaryState :: String -> Maybe CanaryState
parseCanaryState "live"  = Just Live
parseCanaryState "canary"  = Just Canary
parseCanaryState _ = Nothing

canaryStateToString :: CanaryState -> String
canaryStateToString Live = "live"
canaryStateToString Canary = "canary"

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

-- | This combinator transforms a codec over `String` into one that operates on the `PoPName` type.
popName :: RouteDuplex' String -> RouteDuplex' PoPName
popName = as poPNameToString (parsePoPName >>> note "Bad PoPName")

-- | This combinator transforms a codec over `String` into one that operates on the `RtmpStreamName` type.
streamName :: RouteDuplex' String -> RouteDuplex' RtmpStreamName
streamName = as streamNameToString (parseRtmpStreamName >>> note "Bad RtmpStreamName")

-- | This combinator transforms a codec over `String` into one that operates on the `RtmpShortName` type.
shortName :: RouteDuplex' String -> RouteDuplex' RtmpShortName
shortName = as shortNameToString (parseRtmpShortName >>> note "Bad RtmpShortName")

-- | This combinator transforms a codec over `String` into one that operates on the `Canary` type.
canary :: RouteDuplex' String -> RouteDuplex' CanaryState
canary = as canaryStateToString (parseCanaryState >>> note "Bad CanaryId")

uName :: RouteDuplex' String -> RouteDuplex' Username
uName = as userNametoString (parseUsername >>> note "Bad username")
