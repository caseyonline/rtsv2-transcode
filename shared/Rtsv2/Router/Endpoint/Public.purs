module Shared.Rtsv2.Router.Endpoint.Public where

import Prelude hiding ((/))

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Newtype (class Newtype, wrap)
import Routing.Duplex (RouteDuplex', print, rest, root, segment)
import Routing.Duplex.Generic (noArgs, sum)
import Routing.Duplex.Generic.Syntax ((/))
import Shared.Common (Url)
import Shared.Rtsv2.Router.Endpoint.Combinators (shortName, slotId, slotRole, streamName, uName)
import Shared.Rtsv2.Stream (RtmpShortName, RtmpStreamName, SlotId, SlotRole)
import Shared.Rtsv2.Types (ServerAddress(..), Username, extractAddress)

data Endpoint
  =
  -- Public
    StreamDiscoveryE String String
  | ClientPlayerE SlotId SlotRole
  | ClientPlayerControlE SlotId SlotRole
  | ClientPlayerAssetsE SlotId SlotRole (Array String)

  | ClientWebRTCIngestE RtmpShortName RtmpStreamName
  | ClientWebRTCIngestControlE RtmpShortName RtmpStreamName
  | ClientWebRTCIngestAssetsE RtmpShortName RtmpStreamName (Array String)

derive instance genericEndpoint :: Generic Endpoint _

instance showEndpoint :: Show Endpoint where
  show = genericShow

-- | Our codec will cause a compile-time error if we fail to handle any of our route cases.
endpoint :: RouteDuplex' Endpoint
endpoint = root $ sum
  {
  -- Public
    "StreamDiscoveryE"                                 : "public" / "discovery" / "v1" / segment / segment

  , "ClientPlayerE"                                    : "public" / "client" / slotId segment / slotRole segment / "player"
  , "ClientPlayerControlE"                             : "public" / "client" / slotId segment / slotRole segment / "session" -- URL duplicated in Web.purs
  , "ClientPlayerAssetsE"                              : "public" / "client" / slotId segment / slotRole segment / rest

  , "ClientWebRTCIngestE"                              : "public" / "ingest" / shortName segment / streamName segment / "ingest"
  , "ClientWebRTCIngestControlE"                       : "public" / "ingest" / shortName segment / streamName segment / "session"
  , "ClientWebRTCIngestAssetsE"                        : "public" / "ingest" / shortName segment / streamName segment / rest
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
