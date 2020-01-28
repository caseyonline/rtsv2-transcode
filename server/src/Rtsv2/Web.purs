module Rtsv2.Web
  ( startLink
  )
  where

import Prelude

import Data.Newtype (wrap)
import Effect (Effect)
import Erl.Atom (Atom, atom)
import Erl.Cowboy.Req (Req)
import Erl.Cowboy.Routes (InitialState(..), Path(..), matchSpec)
import Erl.Data.List (List, nil, singleton, (:))
import Erl.Data.Tuple (Tuple2, Tuple4, tuple2, tuple3, tuple4, uncurry4)
import Erl.ModuleName (NativeModuleName(..))
import Foreign (unsafeToForeign)
import Logger (Logger)
import Logger as Logger
import Pinto (ServerName, StartLinkResult)
import Pinto.Gen as Gen
import Rtsv2.Config as Config
import Rtsv2.Env as Env
import Rtsv2.Handler.Client as ClientHandler
import Rtsv2.Handler.EgestStats as EgestStatsHandler
import Rtsv2.Handler.Health as HealthHandler
import Rtsv2.Handler.Ingest as IngestHandler
import Rtsv2.Handler.IngestAggregator as IngestAggregatorHandler
import Rtsv2.Handler.LlnwStub as LlnwStubHandler
import Rtsv2.Handler.Load as LoadHandler
import Rtsv2.Handler.Relay as RelayHandler
import Rtsv2.Handler.TransPoP as TransPoPHandler
import Rtsv2.Names as Names
import Rtsv2.Router.Endpoint (Endpoint(..), endpoint)
import Rtsv2.Router.Parser (printUrl)
import Serf (Ip(..))
import Shared.Stream (StreamAndVariant(..), StreamId(..), StreamVariant(..))
import Shared.Types (ServerAddress)
import Stetson (InnerStetsonHandler, RestResult, StaticAssetLocation(..), StetsonHandler, StetsonConfig)
import Stetson as Stetson
import Stetson.Rest as Rest

newtype State = State {}


serverName :: ServerName State Unit
serverName = Names.webServerName

startLink :: Config.WebConfig -> Effect StartLinkResult
startLink args =
  Gen.startLink serverName (init args) Gen.defaultHandleInfo


init :: Config.WebConfig -> Effect State
init args = do
  bindIp <- Env.privateInterfaceIp
  Stetson.configure
    # mkRoute  TransPoPLeaderE                                                                       TransPoPHandler.leader
    # mkRoute  HealthCheckE                                                                          HealthHandler.healthCheck
    # mkRoute (EgestStatsE (StreamId ":stream_id"))                                                  EgestStatsHandler.stats
    # mkRoute  RelayE                                                                                RelayHandler.resource
    # mkRoute (RelayStatsE(StreamId ":stream_id"))                                                   RelayHandler.stats
    # mkRoute  LoadE                                                                                 LoadHandler.load
    # mkRoute (IngestAggregatorE $ StreamId ":stream_id")                                            IngestAggregatorHandler.ingestAggregator
    # mkRoute (IngestAggregatorActiveIngestsE (StreamId ":stream_id") (StreamVariant ":variant_id")) IngestAggregatorHandler.ingestAggregatorsActiveIngest
    # mkRoute  IngestAggregatorsE                                                                    IngestAggregatorHandler.ingestAggregators
    # mkRoute (IngestStartE ":canary" ":short_name" (StreamVariant ":variant_id"))                   IngestHandler.ingestStart
    # mkRoute (IngestStopE ":canary" ":short_name" (StreamVariant ":variant_id"))                    IngestHandler.ingestStop
    # mkRoute (ClientStartE ":canary" (StreamId ":stream_id"))                                       ClientHandler.clientStart
    # mkRoute (ClientStopE ":canary" (StreamId ":stream_id"))                                        ClientHandler.clientStop
    # mkRoute  StreamAuthE                                                                           LlnwStubHandler.streamAuthType
    # mkRoute  StreamAuthTypeE                                                                       LlnwStubHandler.streamAuth
    # mkRoute  StreamPublishE                                                                        LlnwStubHandler.streamPublish

    # static  (IngestAggregatorPlayerE (StreamId ":stream_id"))                                                       (PrivFile "rtsv2" "www/aggregatorPlayer.html")
    # static' (IngestAggregatorPlayerJsE (StreamId ":stream_id")) "/[...]"                                            (PrivDir "rtsv2" "www/assets/js")
    # static  (IngestAggregatorActiveIngestsPlayerE (StreamId ":stream_id") (StreamVariant ":variant_id"))            (PrivFile "rtsv2" "www/play.html")
    # static' (IngestAggregatorActiveIngestsPlayerJsE (StreamId ":stream_id") (StreamVariant ":variant_id")) "/[...]" (PrivDir "rtsv2" "www/assets/js")

    # static' (ClientAppAssets) "/[...]"    (PrivDir Config.appName "www/assets")
    # static  (ClientAppRouteHTML)          (PrivFile Config.appName "www/index.html")
    # static' (ClientAppRouteHTML) "/[...]" (PrivFile Config.appName "www/index.html")

    # Stetson.cowboyRoutes cowboyRoutes
    # Stetson.port args.port
    # (uncurry4 Stetson.bindTo) (ipToTuple bindIp)
    # Stetson.startClear "http_listener"
  pure $ State {}
  where
    cowboyRoutes :: List Path
    cowboyRoutes =
      cowboyRoute   (IngestInstanceLlwpE (StreamId ":stream_id") (StreamVariant ":variant_id"))                              "llwp_stream_resource" makeStreamAndVariant
      : cowboyRoute (IngestAggregatorActiveIngestsPlayerSessionStartE (StreamId ":stream_id") (StreamVariant ":variant_id")) "rtsv2_webrtc_session_start_resource" makeStreamAndVariant
      : cowboyRoute (IngestAggregatorActiveIngestsPlayerSessionE (StreamId ":stream_id") (StreamVariant ":variant_id")       ":session_id") "rtsv2_webrtc_session_resource" makeStreamAndVariant
      : nil

    makeStreamAndVariant :: String -> String -> StreamAndVariant
    makeStreamAndVariant streamId variantId = StreamAndVariant (wrap streamId) (wrap variantId)

    mkRoute :: forall state msg.  Endpoint -> InnerStetsonHandler msg state -> StetsonConfig -> StetsonConfig
    mkRoute rType handler = Stetson.route (printUrl endpoint rType) handler

    static :: Endpoint -> StaticAssetLocation -> StetsonConfig -> StetsonConfig
    static rType config = Stetson.static  (printUrl endpoint rType) config

    static' :: Endpoint -> String -> StaticAssetLocation -> StetsonConfig -> StetsonConfig
    static' rType hack config = Stetson.static ((printUrl endpoint rType) <> hack) config

    cowboyRoute rType moduleName initialState =
      Path (tuple3
            (matchSpec $ printUrl endpoint rType)
            (NativeModuleName $ atom moduleName)
            (InitialState $ unsafeToForeign initialState)
           )

ipToTuple :: Ip -> Tuple4 Int Int Int Int
ipToTuple (Ipv4 a b c d) = tuple4 a b c d

emptyText  :: forall a. Tuple2 String (Req -> a -> (Effect (RestResult String a)))
emptyText = textWriter ""

textWriter :: forall a. String -> Tuple2 String (Req -> a -> (Effect (RestResult String a)))
textWriter text = tuple2 "text/plain" (\req state -> Rest.result text req state)


--------------------------------------------------------------------------------
-- Log helpers
--------------------------------------------------------------------------------
domains :: List Atom
domains = serverName # Names.toDomain # singleton

logInfo :: forall a. Logger a
logInfo = domainLog Logger.info

--logWarning :: forall a. Logger a
--logWarning = domainLog Logger.warning

domainLog :: forall a. Logger {domain :: List Atom, misc :: a} -> Logger a
domainLog = Logger.doLog domains
