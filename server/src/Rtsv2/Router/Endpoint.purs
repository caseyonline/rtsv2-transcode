module Rtsv2.Router.Endpoint ( Endpoint(..)
                             , Canary(..)
                             , endpoint ) where

import Prelude hiding ((/))

import Data.Either (note)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Routing.Duplex (RouteDuplex', as, path, root, segment)
import Routing.Duplex.Generic (noArgs, sum)
import Routing.Duplex.Generic.Syntax ((/))
import Shared.Stream (StreamId(..), StreamVariant(..))

-- data Canary = Live
--             | Canary

type Canary = String

data Endpoint
  = TransPoPLeaderE
  | HealthCheckE
  | EgestStatsE StreamId
  | RelayE StreamId
  | LoadE
  | IngestAggregatorE StreamId
  | IngestAggregatorActiveIngestsE String StreamVariant
  | IngestAggregatorsE
  | IngestStartE Canary String StreamVariant
  | IngestStopE Canary String StreamVariant
  | ClientStartE Canary StreamId
  | ClientStopE Canary StreamId
  | StreamAuthE
  | StreamAuthTypeE
  | StreamPublishE

derive instance genericEndpoint :: Generic Endpoint _

-- | Our codec will cause a compile-time error if we fail to handle any of our route cases.
endpoint :: RouteDuplex' Endpoint
endpoint = root $ sum
  { "TransPoPLeaderE"                : "" / "api" / path "transPoPLeader" noArgs
  , "HealthCheckE"                   : "" / "api" / path "healthCheck" noArgs
  , "EgestStatsE"                    : "" / "api" / "agents" / "egest" / streamId segment
  , "RelayE"                         : "" / "api" / "agents" / "relay" / streamId segment
  , "LoadE"                          : "" / "api" / path "load" noArgs
  , "IngestAggregatorE"              : "" / "api" / "agents" / "ingestAggregator" / streamId segment
  , "IngestAggregatorActiveIngestsE" : "" / "api" / "agents" / "ingestAggregator" / segment / "activeIngests" / variant segment
  , "IngestAggregatorsE"             : "" / "api" / "agents" / path "ingestAggregator" noArgs

  , "IngestStartE"                   : "" / "api" / "public" / canary segment / "ingest" / segment / variant segment / "start"
  , "IngestStopE"                    : "" / "api" / "public" / canary segment / "ingest" / segment / variant segment / "stop"
  , "ClientStartE"                   : "" / "api" / "public" / canary segment / "client" / streamId segment / "start"
  , "ClientStopE"                    : "" / "api" / "public" / canary segment / "client" / streamId segment / "stop"

  , "StreamAuthE"                    : "" / "llnwstub" / "rts" / "v1" / path "streamauthtype" noArgs
  , "StreamAuthTypeE"                : "" / "llnwstub" / "rts" / "v1" / path "streamauth" noArgs
  , "StreamPublishE"                 : "" / "llnwstub" / "rts" / "v1" / path "streampublish" noArgs
  }

-- | StreamId

parseStreamId :: String -> Maybe StreamId
parseStreamId ""  = Nothing
parseStreamId str = Just (StreamId str)

streamIdToString :: StreamId -> String
streamIdToString = unwrap

-- | StreamVariant
parseStreamVariant :: String -> Maybe StreamVariant
parseStreamVariant  ""  = Nothing
parseStreamVariant  str = Just (StreamVariant str)

variantToString :: StreamVariant -> String
variantToString (StreamVariant str) = str

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

-- | This combinator transforms a codec over `String` into one that operates on the `Canary` type.
canary :: RouteDuplex' String -> RouteDuplex' Canary
--canary = as canaryToString (parseCanary >>> note "Bad CanaryId")
canary = as identity (Just >>> note "Bad CanaryId")
