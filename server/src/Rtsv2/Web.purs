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
import Logger (Logger, spy)
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
import Rtsv2.Handler.IntraPoP as IntraPoPHandler
import Rtsv2.Handler.LlnwStub as LlnwStubHandler
import Rtsv2.Handler.Load as LoadHandler
import Rtsv2.Handler.Relay as RelayHandler
import Rtsv2.Handler.TransPoP as TransPoPHandler
import Rtsv2.Names as Names
import Rtsv2.Router.Endpoint (Endpoint(..), endpoint)
import Rtsv2.Router.Parser (printUrl)
import Rtsv2.Web.Bindings as Bindings
import Serf (Ip(..))
import Shared.Stream (ShortName(..), StreamAndVariant(..), StreamId(..), StreamVariant(..))
import Shared.Types (PoPName)
import Stetson (InnerStetsonHandler, RestResult, StaticAssetLocation(..), StetsonConfig)
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
    # mkRoute  TransPoPLeaderE                                                  IntraPoPHandler.leader
    # mkRoute (TimedRoutesE popNameBinding)                                     TransPoPHandler.timedRoutes
    # mkRoute  HealthCheckE                                                     HealthHandler.healthCheck
    # mkRoute (EgestStatsE streamIdBinding)                                     EgestStatsHandler.stats

    # mkRoute  RelayE                                                           RelayHandler.startResource
    # mkRoute  RelayEnsureStartedE                                              RelayHandler.ensureStarted
    # mkRoute  RelayRegisterE                                                   RelayHandler.registerEgest
--    # mkRoute  RelayChainE                                                      RelayHandler.chainResource
    # mkRoute (RelayStatsE streamIdBinding)                                     RelayHandler.stats

    # mkRoute  LoadE                                                            LoadHandler.load

    # mkRoute (IngestAggregatorE streamIdBinding)                               IngestAggregatorHandler.ingestAggregator
    # mkRoute (IngestAggregatorActiveIngestsE streamIdBinding variantBinding)   IngestAggregatorHandler.ingestAggregatorsActiveIngest
    # mkRoute  IngestAggregatorsE                                               IngestAggregatorHandler.ingestAggregators

    # mkRoute (IngestInstancesE)                                                IngestHandler.ingestInstances
    # mkRoute (IngestInstanceE streamIdBinding variantBinding)                  IngestHandler.ingestInstance

    # mkRoute (IngestStartE ":canary" shortNameBinding streamAndVariantBinding) IngestHandler.ingestStart
    # mkRoute (IngestStopE ":canary" shortNameBinding streamAndVariantBinding)  IngestHandler.ingestStop

    # mkRoute (ClientStartE ":canary" streamIdBinding)                          ClientHandler.clientStart
    # mkRoute (ClientStopE ":canary" streamIdBinding)                           ClientHandler.clientStop

    # mkRoute  StreamAuthE                                                      LlnwStubHandler.streamAuthType
    # mkRoute  StreamAuthTypeE                                                  LlnwStubHandler.streamAuth
    # mkRoute  StreamPublishE                                                   LlnwStubHandler.streamPublish

    # static  (IngestAggregatorPlayerE streamIdBinding)                                        (PrivFile "rtsv2" "www/aggregatorPlayer.html")
    # static' (IngestAggregatorPlayerJsE streamIdBinding)                             "/[...]" (PrivDir "rtsv2" "www/assets/js")
    # static  (IngestAggregatorActiveIngestsPlayerE streamIdBinding variantBinding)            (PrivFile "rtsv2" "www/play.html")
    # static' (IngestAggregatorActiveIngestsPlayerJsE streamIdBinding variantBinding) "/[...]" (PrivDir "rtsv2" "www/assets/js")

    # static' (ClientAppAssetsE) "/[...]"    (PrivDir Config.appName "www/assets")
    # static  (ClientAppRouteHTMLE)          (PrivFile Config.appName "www/index.html")
    # static' (ClientAppRouteHTMLE) "/[...]" (PrivFile Config.appName "www/index.html")

    # Stetson.cowboyRoutes cowboyRoutes
    # Stetson.port args.port
    # (uncurry4 Stetson.bindTo) (ipToTuple bindIp)
    # Stetson.startClear "http_listener"
  pure $ State {}
  where
    cowboyRoutes :: List Path
    cowboyRoutes =
      cowboyRoute   (IngestInstanceLlwpE streamIdBinding variantBinding)                                       "llwp_stream_resource" ((unsafeToForeign) makeStreamAndVariant)
      : cowboyRoute (IngestAggregatorActiveIngestsPlayerSessionStartE streamIdBinding variantBinding)          "rtsv2_webrtc_session_start_resource" ((unsafeToForeign) makeStreamAndVariant)
      : cowboyRoute (IngestAggregatorActiveIngestsPlayerSessionE streamIdBinding variantBinding ":session_id") "rtsv2_webrtc_session_resource" ((unsafeToForeign) makeStreamAndVariant)
      : cowboyRoute WorkflowsE "id3as_workflows_resource" (unsafeToForeign unit)

      : cowboyRoute (WorkflowGraphE ":reference") "id3as_workflow_graph_resource" (unsafeToForeign (atom "graph"))
      : cowboyRoute (WorkflowMetricsE ":reference") "id3as_workflow_graph_resource" (unsafeToForeign (atom "metrics"))
      : cowboyRoute (WorkflowStructureE ":reference") "id3as_workflow_graph_resource" (unsafeToForeign (atom "structure"))

      : nil

    makeStreamAndVariant :: String -> String -> StreamAndVariant
    makeStreamAndVariant streamId variantId = StreamAndVariant (wrap streamId) (wrap variantId)

    mkRoute :: forall state msg.  Endpoint -> InnerStetsonHandler msg state -> StetsonConfig -> StetsonConfig
    mkRoute rType handler = Stetson.route (spy "route" (printUrl endpoint rType)) handler

    static :: Endpoint -> StaticAssetLocation -> StetsonConfig -> StetsonConfig
    static rType config = Stetson.static  (printUrl endpoint rType) config

    static' :: Endpoint -> String -> StaticAssetLocation -> StetsonConfig -> StetsonConfig
    static' rType hack config = Stetson.static ((printUrl endpoint rType) <> hack) config

    streamIdBinding = StreamId (":" <> Bindings.streamIdBindingLiteral)
    variantBinding = StreamVariant (":" <> Bindings.variantBindingLiteral)
    streamAndVariantBinding = StreamAndVariant (StreamId "ignored") (StreamVariant $ ":" <> Bindings.streamAndVariantBindingLiteral)
    shortNameBinding = ShortName (":" <> Bindings.shortNameBindingLiteral)

    popNameBinding :: PoPName
    popNameBinding = wrap (":" <> Bindings.popNameBindingLiteral)

    cowboyRoute rType moduleName initialState =
      Path (tuple3
            (matchSpec $ printUrl endpoint rType)
            (NativeModuleName $ atom moduleName)
            (InitialState $ initialState)
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
