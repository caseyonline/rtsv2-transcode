module Rtsv2.Web
  ( startLink
  )
  where

import Prelude

import Data.Function.Uncurried (mkFn2)
import Data.Int (fromString)
import Data.Maybe (fromMaybe)
import Data.Newtype (wrap)
import Effect (Effect)
import Erl.Atom (Atom, atom)
import Erl.Cowboy.Req (Req)
import Erl.Cowboy.Routes (InitialState(..), Path(..), matchSpec)
import Erl.Data.List (List, nil, singleton, (:))
import Erl.Data.Tuple (Tuple2, Tuple4, tuple2, tuple3, tuple4, uncurry4)
import Erl.ModuleName (NativeModuleName(..))
import Erl.Process.Raw (Pid)
import Foreign (unsafeToForeign)
import Logger (Logger, spy)
import Logger as Logger
import Pinto (ServerName, StartLinkResult)
import Pinto.Gen as Gen
import Rtsv2.Agents.EgestInstance as EgestInstance
import Rtsv2.Agents.Locator.Egest (findEgest)
import Rtsv2.Agents.Locator.Types (LocationResp, RegistrationResp)
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
import Rtsv2.Router.Endpoint (Endpoint(..), Canary, endpoint)
import Rtsv2.Router.Endpoint as Router
import Rtsv2.Router.Parser (printUrl)
import Rtsv2.Web.Bindings as Bindings
import Serf (Ip(..))
import Shared.Stream (EgestKey(..), IngestKey(..), ProfileName(..), RtmpShortName(..), SlotId, SlotIdAndProfileName(..), SlotNameAndProfileName(..), SlotRole(..))
import Shared.Types (PoPName)
import Stetson (InnerStetsonHandler, RestResult, StaticAssetLocation(..), StetsonConfig)
import Stetson as Stetson
import Stetson.Rest as Rest
import Stetson.Routing (dummyHandler)
import Stetson.Types (CowboyRoutePlaceholder(..))
import Unsafe.Coerce (unsafeCoerce)

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
    # Stetson.routes
        Router.endpoint
        { "VMMetricsE": HealthHandler.vmMetrics
        , "TransPoPLeaderE": IntraPoPHandler.leader
        , "IntraPoPTestHelperE": IntraPoPHandler.testHelper
        , "TimedRoutesE": TransPoPHandler.timedRoutes
        , "HealthCheckE": HealthHandler.healthCheck
        , "ServerStateE": IntraPoPHandler.publicState
        , "PoPDefinitionE": PoPDefinitionHandler.popDefinition
        , "LoadE": LoadHandler.load

        , "EgestStatsE": EgestStatsHandler.stats
        , "EgestE": dummyHandler -- TODO missing?

        , "RelayE": RelayHandler.startResource
        , "RelayEnsureStartedE": RelayHandler.ensureStarted
        , "RelayRegisterEgestE": RelayHandler.registerEgest
        , "RelayRegisterRelayE": RelayHandler.registerRelay
        , "RelayProxiedStatsE": RelayHandler.proxiedStats
        , "RelayStatsE": RelayHandler.stats
        , "RelaySlotConfigurationE": RelayHandler.slotConfiguration

        , "IngestAggregatorE": IngestAggregatorHandler.ingestAggregator
        , "IngestAggregatorPlayerE": \(_ :: SlotId) (_ :: SlotRole) -> PrivFile "rtsv2" "www/aggregatorPlayer.html"
        , "IngestAggregatorPlayerJsE": \(_ :: SlotId) (_ :: SlotRole) -> PrivDir "rtsv2" "www/assets/js"
        , "IngestAggregatorActiveIngestsE": IngestAggregatorHandler.ingestAggregatorsActiveIngest
        , "IngestAggregatorActiveIngestsPlayerE": \(_ :: SlotId) (_ :: SlotRole) (_ :: ProfileName) -> PrivFile "rtsv2" "www/play.html"
        , "IngestAggregatorActiveIngestsPlayerJsE": \(_ :: SlotId) (_ :: SlotRole) (_ :: ProfileName) -> PrivDir "rtsv2" "www/assets/js"
        , "IngestAggregatorActiveIngestsPlayerControlE": CowboyRoutePlaceholder
        , "IngestAggregatorSlotConfigurationE": IngestAggregatorHandler.slotConfiguration
        , "IngestAggregatorRegisterRelayE": IngestAggregatorHandler.registerRelay
        , "IngestAggregatorsE": IngestAggregatorHandler.ingestAggregators
        , "IngestInstancesE": IngestHandler.ingestInstances
        , "IngestInstancesMetricsE": IngestHandler.ingestInstancesMetrics
        , "IngestInstanceE": IngestHandler.ingestInstance
        , "IngestInstanceLlwpE": CowboyRoutePlaceholder
        , "IngestStartE": IngestHandler.ingestStart
        , "IngestStopE": IngestHandler.ingestStop

        , "ClientAppAssetsE": PrivDir Config.appName "www/assets"
        , "ClientAppRouteHTMLE": PrivFile Config.appName "www/index.html"

        , "ClientStartE": ClientHandler.clientStart
        , "ClientStopE": ClientHandler.clientStop
        , "ClientPlayerE":  \(_ :: Canary) (_ :: SlotId) -> PrivFile "rtsv2" "www/egestReferencePlayer.html"
        , "ClientPlayerJsE": \(_ :: Canary) (_ :: SlotId) (_ :: Array String) -> PrivDir "rtsv2" "www/assets/js"

        , "StreamAuthE": LlnwStubHandler.streamAuth
        , "StreamAuthTypeE": LlnwStubHandler.streamAuthType
        , "StreamPublishE": LlnwStubHandler.streamPublish

        , "WorkflowsE": CowboyRoutePlaceholder -- id3as_workflows_resource
        , "WorkflowGraphE": CowboyRoutePlaceholder -- id3as_workflow_graph_resource
        , "WorkflowMetricsE": CowboyRoutePlaceholder --id3as_workflow_graph_resource
        , "WorkflowStructureE": CowboyRoutePlaceholder -- id3as_workflow_graph_resource

        }
    # Stetson.cowboyRoutes cowboyRoutes
    # Stetson.port args.port
    # (uncurry4 Stetson.bindTo) (ipToTuple bindIp)
    # Stetson.startClear "http_listener"
  pure $ State {}

  where
    cowboyRoutes :: List Path
    cowboyRoutes =
   -- cowboyRoute   (IngestInstanceLlwpE slotIdBinding streamRoleBinding profileNameBinding)                                       "llwp_stream_resource" ((unsafeToForeign) makeSlotIdAndProfileName)
      cowboyHack   "/api/agents/ingest/:slot_id/:stream_role/:profile_name/llwp"                                  "llwp_stream_resource" ((unsafeToForeign) makeSlotIdAndProfileName)

      -- : cowboyRoute (IngestAggregatorActiveIngestsPlayerControlE slotIdBinding profileNameBinding)               "rtsv2_player_ws_resource" ((unsafeToForeign) makeSlotIdAndProfileName)
      : cowboyHack "/api/agents/ingestAggregator/:slot_id/:stream_role/activeIngests/:profile_name/session"    "rtsv2_player_ws_resource" (unsafeToForeign { mode: (atom "ingest"), make_ingest_key: makeIngestKey })

      : cowboyHack "/api/public/:canary/client/:slot_id/session"                                             "rtsv2_player_ws_resource" (unsafeToForeign { mode: (atom "egest"), make_egest_key: EgestKey, start_stream: startStream, add_client: mkFn2 addClient, get_slot_configuration: EgestInstance.slotConfiguration })

      : cowboyRoute WorkflowsE "id3as_workflows_resource" (unsafeToForeign unit)
      : cowboyRoute (WorkflowGraphE ":reference") "id3as_workflow_graph_resource" (unsafeToForeign (atom "graph"))
      : cowboyRoute (WorkflowMetricsE ":reference") "id3as_workflow_graph_resource" (unsafeToForeign (atom "metrics"))
      : cowboyRoute (WorkflowStructureE ":reference") "id3as_workflow_graph_resource" (unsafeToForeign (atom "structure"))

      : nil

    makeSlotIdAndProfileName :: String -> String -> SlotIdAndProfileName
    makeSlotIdAndProfileName slotId profileName = SlotIdAndProfileName (slotIdStringToSlotId slotId) (wrap profileName)

    makeIngestKey :: Int -> String -> String -> IngestKey
    makeIngestKey slotId streamRole profileName =
      IngestKey (wrap slotId) (parseSlotRole streamRole) (wrap profileName)
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
        findEgest (wrap slotId) thisServer

    addClient :: Pid -> Int -> Effect RegistrationResp
    addClient pid slotId =
      EgestInstance.addClient pid (EgestKey (wrap slotId))


-- =======
--     mkRoute :: forall state msg.  Endpoint -> InnerStetsonHandler msg state -> StetsonConfig -> StetsonConfig
--     mkRoute rType handler = Stetson.route (spy "route" (printUrl endpoint rType)) handler

--     mkHack :: forall state msg. String -> InnerStetsonHandler msg state -> StetsonConfig -> StetsonConfig
--     mkHack path  = Stetson.route (spy "route" path)

--     static :: Endpoint -> StaticAssetLocation -> StetsonConfig -> StetsonConfig
--     static rType config = Stetson.static  (printUrl endpoint rType) config

--     static' :: Endpoint -> String -> StaticAssetLocation -> StetsonConfig -> StetsonConfig
--     static' rType hack config = Stetson.static ((printUrl endpoint rType) <> hack) config

--     staticHack :: String -> StaticAssetLocation -> StetsonConfig -> StetsonConfig
--     staticHack path config = Stetson.static  (spy "route" path) config

--     profileNameBinding = ProfileName (":" <> Bindings.profileNameBindingLiteral)

--     slotNameAndProfileName = SlotNameAndProfileName "ignored" (ProfileName $ ":" <> Bindings.slotNameAndProfileNameBindingLiteral)

--     shortNameBinding = RtmpShortName (":" <> Bindings.shortNameBindingLiteral)
-- >>>>>>> master

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
