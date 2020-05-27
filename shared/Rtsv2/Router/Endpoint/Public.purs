module Shared.Rtsv2.Router.Endpoint.Public where

import Prelude hiding ((/))

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Newtype (class Newtype, wrap)
import Effect (Effect)
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
  pure $ wrap $ "http://" <> host <> ":" <> (show webC.publicPort) <> path

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
  pure $ wrap $ "ws://" <> host <> ":" <> (show webC.publicPort) <> path
