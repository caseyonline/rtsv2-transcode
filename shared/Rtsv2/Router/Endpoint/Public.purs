module Shared.Rtsv2.Router.Endpoint.Public where

import Prelude hiding ((/))

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Newtype (class Newtype, wrap)
import Routing.Duplex (RouteDuplex', print, rest, root, segment)
import Routing.Duplex.Generic (noArgs, sum)
import Routing.Duplex.Generic.Syntax ((/))
import Rtsv2.Config as Config
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

makeUrl
  :: forall r a. Newtype a { address :: ServerAddress | r }
  => a
  -> Config.PortInt
  -> Endpoint
  -> Url
makeUrl server = makeUrlAddr (extractAddress server)

makeUrlWithPath
  :: forall r a. Newtype a { address :: ServerAddress | r }
  => a
  -> Config.PortInt
  -> String
  -> Url
makeUrlWithPath server = makeUrlAddrWithPath (extractAddress server)

makeUrlAddr :: ServerAddress -> Config.PortInt -> Endpoint -> Url
makeUrlAddr serverAddr portInt ep =
  makeUrlAddrWithPath serverAddr portInt (makePath ep)

makeUrlAddrWithPath :: ServerAddress -> Config.PortInt -> String -> Url
makeUrlAddrWithPath (ServerAddress host) portInt path = do
  wrap $ "http://" <> host <> (show portInt) <> path

makeWsUrl
  :: forall r a. Newtype a { address :: ServerAddress | r }
  => a
  -> Config.PortInt
  -> Endpoint -> Url
makeWsUrl server portInt ep = makeWsUrlAddr (extractAddress server) portInt ep

makeWsUrlWithPath
  :: forall r a. Newtype a { address :: ServerAddress | r }
  => a
  -> Config.PortInt
  -> String
  -> Url
makeWsUrlWithPath server = makeWsUrlAddrWithPath (extractAddress server)

makeWsUrlAddr :: ServerAddress -> Config.PortInt -> Endpoint -> Url
makeWsUrlAddr serverAddr portInt ep = do
  makeWsUrlAddrWithPath serverAddr portInt (makePath ep)

makeWsUrlAddrWithPath :: ServerAddress -> Config.PortInt -> String -> Url
makeWsUrlAddrWithPath (ServerAddress host) portInt path = do
  wrap $ "ws://" <> host <> (show portInt) <> path
