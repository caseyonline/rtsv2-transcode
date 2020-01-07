module Rtsv2.Router where

import Prelude hiding ((/))

import Data.Either (note)
import Data.Function (applyFlipped)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Ord (genericCompare)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.String (joinWith)
import Data.String.CodeUnits (contains)
import Data.String.Pattern (Pattern(..))
import Data.Tuple (uncurry)
import Global.Unsafe (unsafeEncodeURIComponent)
import Routing.Duplex (RouteDuplex(..), RouteDuplex', as, path, root, segment)
import Routing.Duplex.Generic (noArgs, sum)
import Routing.Duplex.Generic.Syntax ((/))
import Routing.Duplex.Printer (RoutePrinter)
import Routing.Duplex.Types (RouteState, emptyRouteState)

type Canary = String
data StreamId = StreamId String
data StreamVariant = StreamVariant String

data Endpoint
  = TransPoPLeaderE
  | HealthCheckE
  | LoadE
  | IngestAggregator StreamId
  | IngestStartE Canary StreamId StreamVariant
  | IngestStopE Canary StreamId StreamVariant
  | ClientStartE Canary StreamId
  | ClientStopE Canary StreamId
  | ClientCountE Canary StreamId
  | StreamAuthE
  | StreamAuthTypeE
  | StreamPublishE

derive instance genericEndpoint :: Generic Endpoint _

-- | Our codec will cause a compile-time error if we fail to handle any of our route cases.
endpoint :: RouteDuplex' Endpoint
endpoint = root $ sum
  { "TransPoPLeaderE" : "" / "api" / path "transPoPLeader" noArgs
  , "HealthCheckE"    : "" / "api" / path "healthCheck" noArgs
  , "LoadE"           : "" / "api" / path "load" noArgs
  , "IngestAggregator": "" / "api" / "agents" / "ingestAggregator" / streamId segment
  , "IngestStartE"    : "" / "api" / "client" / segment / "ingest" / streamId segment / variant segment / "start"
  , "IngestStopE"     : "" / "api" / "client" / segment / "ingest" / streamId segment / variant segment / "stop"
  , "ClientStartE"    : "" / "api" / "client" / segment / "client" / streamId segment / "start"
  , "ClientStopE"     : "" / "api" / "client" / segment / "client" / streamId segment / "stop"
  , "ClientCountE"    : "" / "api" / "client" / segment / "edge" / streamId segment / "clientCount"
  , "StreamAuthE"     : "" / "llnwstub/" / "rts" / "v1" / path "streamauthtype" noArgs
  , "StreamAuthTypeE" : "" / "llnwstub/" / "rts" / "v1" / path "streamauth" noArgs
  , "StreamPublishE"  : "" / "llnwstub/" / "rts" / "v1" / path "streampublish" noArgs
  }

-- | StreamId
derive instance genericStreamId :: Generic StreamId _

instance eqStreamId :: Eq StreamId where
  eq = genericEq

instance compareStreamId :: Ord StreamId where
  compare = genericCompare

instance showStreamId :: Show StreamId where
  show = genericShow

parseStreamId :: String -> Maybe StreamId
parseStreamId ""  = Nothing
parseStreamId str = Just (StreamId str)

streamIdToString :: StreamId -> String
streamIdToString (StreamId str) = str

-- | StreamVariant
derive instance genericStreamVariant :: Generic StreamVariant _

instance eqStreamVariant :: Eq StreamVariant where
  eq = genericEq

instance compareStreamVariant :: Ord StreamVariant where
  compare = genericCompare

instance showStreamVariant :: Show StreamVariant where
  show = genericShow

parseStreamVariant :: String -> Maybe StreamVariant
parseStreamVariant  ""  = Nothing
parseStreamVariant  str = Just (StreamVariant str)

variantToString :: StreamVariant -> String
variantToString (StreamVariant str) = str


-- | This combinator transforms a codec over `String` into one that operates on the `StreamId` type.
streamId :: RouteDuplex' String -> RouteDuplex' StreamId
streamId = as streamIdToString (parseStreamId >>> note "Bad StreamId")

-- | This combinator transforms a codec over `String` into one that operates on the `StreamVariant` type.
variant :: RouteDuplex' String -> RouteDuplex' StreamVariant
variant = as variantToString (parseStreamVariant >>> note "Bad StreamId")

printUrl :: forall i o. RouteDuplex i o -> i -> String
printUrl (RouteDuplex enc _) = run <<< enc

run :: RoutePrinter -> String
run = printPath <<< applyFlipped emptyRouteState <<< unwrap

printPath :: RouteState -> String
printPath { segments, params, hash: hash' } =
  printSegments segments <> printParams params <> printHash hash'
  where
  printSegments = case _ of
    [""] -> "/"
    xs -> joinWith "/" $ map (\a -> case contains (Pattern ":") a of
                                         true  -> a
                                         false -> unsafeEncodeURIComponent a) xs

  printParams [] = ""
  printParams ps = "?" <> joinWith "&" (uncurry printParam <$> ps)

  printParam key ""  = unsafeEncodeURIComponent key
  printParam key val = unsafeEncodeURIComponent key <> "=" <> unsafeEncodeURIComponent val

  printHash "" = ""
  printHash h  = "#" <> h


-- printme :: String
-- printme = print endpoint (IngestStartE (":canary") (StreamId ":stream_id") (StreamVariant ":variant_id"))
