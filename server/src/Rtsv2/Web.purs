module Rtsv2.Web
  ( startLink
  )
  where

import Prelude

import Data.Int (fromString)
import Data.Maybe (fromMaybe, fromMaybe')
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
import Rtsv2.Agents.EgestInstance as EgestInstance
import Rtsv2.Agents.Locator.Egest (findEgestAndRegister)
import Rtsv2.Agents.Locator.Types (FailureReason(..), LocalOrRemote(..), LocationResp)
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
import Rtsv2.Handler.PoPDefinition as PoPDefinitionHandler
import Rtsv2.Handler.Relay as RelayHandler
import Rtsv2.Handler.TransPoP as TransPoPHandler
import Rtsv2.Names as Names
import Rtsv2.PoPDefinition as PoPDefinition
import Rtsv2.Router.Endpoint (Endpoint(..), endpoint)
import Rtsv2.Router.Parser (printUrl)
import Rtsv2.Web.Bindings as Bindings
import Serf (Ip(..))
import Shared.Stream (EgestKey(..), IngestKey(..), ProfileName(..), RtmpShortName(..), SlotId(..), SlotIdAndProfileName(..), SlotNameAndProfileName(..), SlotRole(..))
import Shared.Types (PoPName)
import Shared.Utils (lazyCrashIfMissing)
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
    # mkRoute  VMMetricsE                                                       HealthHandler.vmMetrics
    # mkRoute  TransPoPLeaderE                                                  IntraPoPHandler.leader
    # mkRoute  ServerStateE                                                     IntraPoPHandler.publicState
    # mkRoute  IntraPoPTestHelperE                                              IntraPoPHandler.testHelper

    # mkRoute (TimedRoutesE popNameBinding)                                     TransPoPHandler.timedRoutes
    # mkRoute  HealthCheckE                                                     HealthHandler.healthCheck
    # mkRoute  PoPDefinitionE                                                   PoPDefinitionHandler.popDefinition

    -- # mkRoute (EgestStatsE streamIdBinding)                                     EgestStatsHandler.stats
    # mkHack "/api/agents/egest/:stream_id"                                     EgestStatsHandler.stats


    # mkRoute  RelayE                                                           RelayHandler.startResource
    # mkRoute  RelayEnsureStartedE                                              RelayHandler.ensureStarted
    # mkRoute  RelayRegisterEgestE                                              RelayHandler.registerEgest
    # mkRoute  RelayRegisterRelayE                                              RelayHandler.registerRelay
 -- # mkRoute (RelayStatsE streamIdBinding streamRoleBinding)                   RelayHandler.stats
    # mkHack  "/api/agents/relay/:stream_id/:stream_role"                       RelayHandler.stats
 -- # mkRoute (RelayProxiedStatsE streamIdBinding streamRoleBinding)            RelayHandler.proxiedStats
    # mkHack  "/api/agents/proxied/relay/:stream_id/:stream_role"               RelayHandler.proxiedStats

    # mkRoute  LoadE                                                            LoadHandler.load

 -- # mkRoute (IngestAggregatorE streamIdBinding)                               IngestAggregatorHandler.ingestAggregator
    # mkHack  "/api/agents/ingestAggregator/:stream_id/:stream_role"            IngestAggregatorHandler.ingestAggregator
 -- # mkRoute (IngestAggregatorActiveIngestsE streamIdBinding streamRoleBinding variantBinding) IngestAggregatorHandler.ingestAggregatorsActiveIngest
    # mkHack  "/api/agents/ingestAggregator/:stream_id/:stream_role/activeIngests/:variant" IngestAggregatorHandler.ingestAggregatorsActiveIngest
    # mkRoute  IngestAggregatorRegisterRelayE                                   IngestAggregatorHandler.registerRelay
    # mkRoute  IngestAggregatorsE                                               IngestAggregatorHandler.ingestAggregators

    # mkRoute (IngestInstancesE)                                                IngestHandler.ingestInstances
    # mkRoute (IngestInstancesMetricsE)                                         IngestHandler.ingestInstancesMetrics
    -- # mkRoute (IngestInstanceE streamIdBinding variantBinding)                  IngestHandler.ingestInstance
    # mkHack "/api/agents/ingest/:stream_id/:variant"                           IngestHandler.ingestInstance

    # mkRoute (IngestStartE ":canary" shortNameBinding slotNameAndProfileName) IngestHandler.ingestStart
 -- # mkRoute (IngestStopE ":canary" shortNameBinding slotNameAndProfileName)  IngestHandler.ingestStop
    # mkHack  "/api/public/:canary/ingest/:stream_id/:stream_role/:variant/stop" IngestHandler.ingestStop

    -- # mkRoute (ClientStartE ":canary" streamIdBinding)                          ClientHandler.clientStart
    # mkHack  "/api/public/:canary/client/:stream_id/start"                     ClientHandler.clientStart
    -- # mkRoute (ClientStopE ":canary" streamIdBinding)                           ClientHandler.clientStop
    # mkHack  "/api/public/:canary/client/:stream_id/stop"                     ClientHandler.clientStop

    # mkRoute  StreamAuthE                                                      LlnwStubHandler.streamAuthType
    # mkRoute  StreamAuthTypeE                                                  LlnwStubHandler.streamAuth
    # mkRoute  StreamPublishE                                                   LlnwStubHandler.streamPublish

    -- Slot configuration endpoints
    # mkHack  "/api/agents/ingestAggregator/:stream_id/:stream_role/slot"       IngestAggregatorHandler.slotConfiguration
    # mkHack  "/api/agents/relay/:stream_id/:stream_role/slot"                  RelayHandler.slotConfiguration

    # staticHack "/api/agents/ingestAggregator/:stream_id/:stream_role/player"                                   (PrivFile "rtsv2" "www/aggregatorPlayer.html")
    # staticHack "/api/agents/ingestAggregator/:stream_id/:stream_role/js/[...]"                                 (PrivDir "rtsv2" "www/assets/js")
    # staticHack "/api/agents/ingestAggregator/:stream_id/:stream_role/activeIngests/:variant/player"            (PrivFile "rtsv2" "www/unifiedPlayer.html")
    # staticHack "/api/agents/ingestAggregator/:stream_id/:stream_role/activeIngests/:variant/js/[...]"          (PrivDir "rtsv2" "www/assets/js")
    -- # static  (IngestAggregatorPlayerE streamIdBinding)                                        (PrivFile "rtsv2" "www/aggregatorPlayer.html")
    -- # static' (IngestAggregatorPlayerJsE streamIdBinding)                             "/[...]" (PrivDir "rtsv2" "www/assets/js")
    -- # static  (IngestAggregatorActiveIngestsPlayerE streamIdBinding variantBinding)            (PrivFile "rtsv2" "www/unifiedPlayer.html")
    -- # static' (IngestAggregatorActiveIngestsPlayerJsE streamIdBinding variantBinding) "/[...]" (PrivDir "rtsv2" "www/assets/js")

    -- We wouldn't host this in a production system, this is just for reference
    # staticHack "/api/public/:canary/client/:stream_id/player"                (PrivFile "rtsv2" "www/egestReferencePlayer.html")
    # staticHack "/api/public/:canary/client/:stream_id/js/[...]"              (PrivDir "rtsv2" "www/assets/js")

    # Stetson.cowboyRoutes cowboyRoutes

    # static' (ClientAppAssetsE) "/[...]"    (PrivDir Config.appName "www/assets")
    # static  (ClientAppRouteHTMLE)          (PrivFile Config.appName "www/index.html")
    # static' (ClientAppRouteHTMLE) "/[...]" (PrivFile Config.appName "www/index.html")

    # Stetson.port args.port
    # (uncurry4 Stetson.bindTo) (ipToTuple bindIp)
    # Stetson.startClear "http_listener"
  pure $ State {}

  where
    cowboyRoutes :: List Path
    cowboyRoutes =
   -- cowboyRoute   (IngestInstanceLlwpE streamIdBinding streamRoleBinding variantBinding)                                       "llwp_stream_resource" ((unsafeToForeign) makeSlotIdAndProfileName)
      cowboyHack   "/api/agents/ingest/:stream_id/:stream_role/:variant/llwp"                                  "llwp_stream_resource" ((unsafeToForeign) makeSlotIdAndProfileName)

      -- : cowboyRoute (IngestAggregatorActiveIngestsPlayerControlE streamIdBinding variantBinding)               "rtsv2_player_ws_resource" ((unsafeToForeign) makeSlotIdAndProfileName)
      : cowboyHack "/api/agents/ingestAggregator/:stream_id/:stream_role/activeIngests/:variant_id/session"    "rtsv2_player_ws_resource" (unsafeToForeign { mode: (atom "ingest"), make_ingest_key: makeIngestKey })

      : cowboyHack "/api/public/:canary/client/:stream_id/session"                                             "rtsv2_player_ws_resource" (unsafeToForeign { mode: (atom "egest"), make_egest_key: EgestKey, start_stream: startStream, get_slot_configuration: EgestInstance.slotConfiguration })

      : cowboyRoute WorkflowsE "id3as_workflows_resource" (unsafeToForeign unit)

      : cowboyRoute (WorkflowGraphE ":reference") "id3as_workflow_graph_resource" (unsafeToForeign (atom "graph"))
      : cowboyRoute (WorkflowMetricsE ":reference") "id3as_workflow_graph_resource" (unsafeToForeign (atom "metrics"))
      : cowboyRoute (WorkflowStructureE ":reference") "id3as_workflow_graph_resource" (unsafeToForeign (atom "structure"))

      : nil

    makeSlotIdAndProfileName :: String -> String -> SlotIdAndProfileName
    makeSlotIdAndProfileName slotId variantId = SlotIdAndProfileName (slotIdStringToSlotId slotId) (wrap variantId)

    makeIngestKey :: String -> String -> String -> IngestKey
    makeIngestKey slotId streamRole variantId =
      IngestKey (slotIdStringToSlotId slotId) (parseSlotRole streamRole) (wrap variantId)
      where
        parseSlotRole "primary" = Primary
        parseSlotRole "backup" = Backup
        parseSlotRole _ = Primary

    slotIdStringToSlotId :: String -> SlotId
    slotIdStringToSlotId slotIdStr =
      wrap $ fromMaybe 0 (fromString slotIdStr)

    -- TODO: This code doesn't belong here
    startStream :: Int -> Effect LocationResp
    startStream slotId =
      do
        thisServer <- PoPDefinition.getThisServer

        findEgestAndRegister (wrap slotId) thisServer

    mkRoute :: forall state msg.  Endpoint -> InnerStetsonHandler msg state -> StetsonConfig -> StetsonConfig
    mkRoute rType handler = Stetson.route (spy "route" (printUrl endpoint rType)) handler

    mkHack :: forall state msg. String -> InnerStetsonHandler msg state -> StetsonConfig -> StetsonConfig
    mkHack path  = Stetson.route (spy "route" path)

    static :: Endpoint -> StaticAssetLocation -> StetsonConfig -> StetsonConfig
    static rType config = Stetson.static  (printUrl endpoint rType) config

    static' :: Endpoint -> String -> StaticAssetLocation -> StetsonConfig -> StetsonConfig
    static' rType hack config = Stetson.static ((printUrl endpoint rType) <> hack) config

    staticHack :: String -> StaticAssetLocation -> StetsonConfig -> StetsonConfig
    staticHack path config = Stetson.static  (spy "route" path) config

    variantBinding = ProfileName (":" <> Bindings.variantBindingLiteral)

    slotNameAndProfileName = SlotNameAndProfileName "ignored" (ProfileName $ ":" <> Bindings.streamAndVariantBindingLiteral)

    shortNameBinding = RtmpShortName (":" <> Bindings.shortNameBindingLiteral)

    popNameBinding :: PoPName
    popNameBinding = wrap (":" <> Bindings.popNameBindingLiteral)

    cowboyRoute rType moduleName initialState =
      Path (tuple3
            (matchSpec $ printUrl endpoint rType)
            (NativeModuleName $ atom moduleName)
            (InitialState $ initialState)
           )
    cowboyHack path moduleName initialState =
      Path (tuple3
            (matchSpec path)
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
