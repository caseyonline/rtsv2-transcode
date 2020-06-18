module Rtsv2.Web
  ( startLink
  )
  where

import Prelude

import Data.Function.Uncurried (mkFn2, mkFn3, mkFn7)
import Data.Newtype (unwrap, wrap)
import Effect (Effect)
import Erl.Atom (Atom, atom)
import Erl.Cowboy.Req (Req)
import Erl.Cowboy.Routes (InitialState(..), Path(..), matchSpec)
import Erl.Data.List (List, nil, singleton, (:))
import Erl.Data.Tuple (Tuple2, Tuple4, tuple2, tuple3, tuple4, uncurry4)
import Erl.ModuleName (NativeModuleName(..))
import Erl.Utils as Erl
import Foreign (Foreign, unsafeToForeign)
import Logger as Logger
import Pinto (ServerName, StartLinkResult)
import Pinto.Gen (TerminateReason)
import Pinto.Gen as Gen
import Rtsv2.Agents.EgestInstance as EgestInstance
import Rtsv2.Agents.EgestInstanceSup as EgestInstanceSup
import Rtsv2.Agents.IngestWebRTCIngestHandler as IngestWebRTCIngestHandler
import Rtsv2.Config (MediaGatewayFlag(Off))
import Rtsv2.Config as Config
import Rtsv2.Env as Env
import Rtsv2.Handler.Canary as CanaryHandler
import Rtsv2.Handler.Chaos as ChaosHandler
import Rtsv2.Handler.Client as ClientHandler
import Rtsv2.Handler.Egest as EgestHandler
import Rtsv2.Handler.EgestStats as EgestStatsHandler
import Rtsv2.Handler.Health as HealthHandler
import Rtsv2.Handler.Ingest as IngestHandler
import Rtsv2.Handler.IngestAggregator as IngestAggregatorHandler
import Rtsv2.Handler.IntraPoP as IntraPoPHandler
import Rtsv2.Handler.JsonLd as JsonLd
import Rtsv2.Handler.LlnwStub as LlnwStubHandler
import Rtsv2.Handler.Load as LoadHandler
import Rtsv2.Handler.PoPDefinition as PoPDefinitionHandler
import Rtsv2.Handler.Relay as RelayHandler
import Rtsv2.Handler.RunState as RunStateHandler
import Rtsv2.Handler.ServiceUnavailable as ServiceUnavailable
import Rtsv2.Handler.StreamDiscovery as StreamDiscoveryHandler
import Rtsv2.Handler.TransPoP as TransPoPHandler
import Rtsv2.Names as Names
import Rtsv2.PoPDefinition as PoPDefinition
import Serf (Ip(..))
import Shared.Rtsv2.Router.Endpoint.Public as Public
import Shared.Rtsv2.Router.Endpoint.Support as Support
import Shared.Rtsv2.Router.Endpoint.System as System
import Shared.Rtsv2.Stream (EgestKey(..), IngestKey(..), ProfileName, RtmpShortName, RtmpStreamName, SlotId, SlotName, SlotRole(..))
import Shared.Rtsv2.Types (CanaryState(..), Server, WebConfig, extractAddress)
import Shared.UUID (UUID)
import Stetson (RestResult, StaticAssetLocation(..), StetsonHandler)
import Stetson as Stetson
import Stetson.Rest as Rest
import Stetson.Types (CowboyRoutePlaceholder(..))

newtype State = State {}

serverName :: ServerName State Unit
serverName = Names.webServerName

startLink :: CanaryState -> Effect StartLinkResult
startLink canaryState =
  Gen.startLink serverName (init canaryState) Gen.defaultHandleInfo

init :: CanaryState -> Effect State
init canaryState = do
  void $ Erl.trapExit true
  featureFlags <- Config.featureFlags
  loadConfig <- Config.loadConfig
  webConfig <- Config.webConfig
  llnwApiConfig <- Config.llnwApiConfig
  thisServer <- PoPDefinition.getThisServer
  Gen.registerTerminate serverName terminate

  public <- publicRoutes canaryState webConfig loadConfig featureFlags llnwApiConfig thisServer
  support <- supportRoutes canaryState webConfig loadConfig featureFlags llnwApiConfig thisServer
  system <- systemRoutes webConfig loadConfig featureFlags thisServer

  pure $ State {}

terminate :: TerminateReason -> State -> Effect Unit
terminate reason state = do
  logInfo "Web terminating" {reason}
  Stetson.stop "public_http"
  Stetson.stop "support_http"
  Stetson.stop "system_http"

  pure unit

publicRoutes :: CanaryState -> WebConfig -> Config.LoadConfig -> Config.FeatureFlags -> Config.LlnwApiConfig -> Server -> Effect Unit
publicRoutes Live webConfig loadConfig featureFlags@{mediaGateway} llnwApiConfig@{validationUrlWhitelist} thisServer = do
  publicBindIp <- Env.publicListenIp
  void $ Stetson.configure
       # Stetson.routes
           Public.endpoint
             { "StreamDiscoveryE"                            : StreamDiscoveryHandler.discover loadConfig Live
             , "ClientPlayerE"                               : \(_ :: SlotId) (_ :: SlotRole) -> PrivFile "rtsv2" "www/egestReferencePlayer.html"
             , "ClientPlayerAssetsE"                         : \(_ :: SlotId) (_ :: SlotRole) -> PrivDir "rtsv2" "www/assets"
             , "ClientPlayerControlE"                        : CowboyRoutePlaceholder

             , "ClientWebRTCIngestE"                         : \(_ :: RtmpShortName) (_ :: RtmpStreamName) -> PrivFile "rtsv2" "www/ingestReferencePlayer.html"
             , "ClientWebRTCIngestAssetsE"                   : \(_ :: RtmpShortName) (_ :: RtmpStreamName) -> PrivDir "rtsv2" "www/assets"
             , "ClientWebRTCIngestControlE"                  : CowboyRoutePlaceholder
             , "FaviconE"                                    : PrivFile "rtsv2" "www/assets/images/favicon.ico"
             }
       # Stetson.cowboyRoutes publicCowboyRoutes
       # Stetson.port webConfig.publicPort
       # (uncurry4 Stetson.bindTo) (ipToTuple publicBindIp)
       # Stetson.startClear "public_http"
  pure unit

  where
    publicCowboyRoutes :: List Path
    publicCowboyRoutes =
      -- Some duplication of URLs here from those in Endpoint.purs due to current inability to build cowboy-style bindings from stongly-typed parameters
      -- ClientPlayerControlE Canary SlotId
        cowboyRoute ("/public/client/" <> slotIdBinding <> "/" <> slotRoleBinding <> "/session")
                    "rtsv2_player_ws_resource"
                    (playerControlArgs loadConfig webConfig mediaGateway Live validationUrlWhitelist)

      -- ClientWebRTCIngestControlE SlotId SlotRole
      : cowboyRoute ("/public/ingest/" <> accountBinding <> "/" <> streamNameBinding <> "/session")
                    "rtsv2_webrtc_push_ingest_ws_resource"
                    (ingestControlArgs thisServer loadConfig Live)

      : nil

publicRoutes Canary webConfig loadConfig featureFlags@{mediaGateway} llnwApiConfig@{validationUrlWhitelist} thisServer = do
  publicBindIp <- Env.publicListenIp
  void $ Stetson.configure
       # Stetson.routes
           Public.endpoint
             { "StreamDiscoveryE"                            : \(_ :: RtmpShortName) (_ :: SlotName) -> unavailable
             , "ClientPlayerE"                               : \(_ :: SlotId) (_ :: SlotRole) -> unavailable
             , "ClientPlayerAssetsE"                         : \(_ :: SlotId) (_ :: SlotRole) (_ :: Array String) -> unavailable
             , "ClientPlayerControlE"                        : \(_ :: SlotId) (_ :: SlotRole) -> unavailable

             , "ClientWebRTCIngestE"                         : \(_ :: RtmpShortName) (_ :: RtmpStreamName) -> unavailable
             , "ClientWebRTCIngestAssetsE"                   : \(_ :: RtmpShortName) (_ :: RtmpStreamName) (_ :: Array String) -> unavailable
             , "ClientWebRTCIngestControlE"                  : \(_ :: RtmpShortName) (_ :: RtmpStreamName) -> unavailable
             , "FaviconE"                                    : unavailable
             }
       # Stetson.port webConfig.publicPort
       # (uncurry4 Stetson.bindTo) (ipToTuple publicBindIp)
       # Stetson.startClear "public_http"
  pure unit
  where
    unavailable :: (StetsonHandler Unit)
    unavailable = ServiceUnavailable.unavailable

supportRoutes :: CanaryState -> WebConfig -> Config.LoadConfig -> Config.FeatureFlags -> Config.LlnwApiConfig -> Server -> Effect Unit
supportRoutes canaryState webConfig loadConfig featureFlags@{mediaGateway} llnwApiConfig@{validationUrlWhitelist} thisServer = do
  supportBindIp <- Env.supportListenIp
  void $ Stetson.configure
         # Stetson.routes
             Support.endpoint
             { "VMMetricsE"                                  : HealthHandler.vmMetrics
             , "TimedRoutesE"                                : TransPoPHandler.timedRoutes
             , "TimedRoutesForPoPE"                          : TransPoPHandler.timedRoutesForPoP
             , "HealthCheckE"                                : HealthHandler.healthCheck
             , "CanaryE"                                     : CanaryHandler.setCanary
             , "RunStateE"                                   : RunStateHandler.setRunState
             , "ServerStateE"                                : IntraPoPHandler.publicState
             , "SlotStateE"                                  : IntraPoPHandler.slotState
             , "PoPDefinitionE"                              : PoPDefinitionHandler.popDefinition
             , "JsonLdContext"                               : JsonLd.getContextJson
             , "EgestStatsE"                                 : EgestStatsHandler.stats thisServer
             , "EgestInstancesMetricsE"                      : EgestHandler.egestInstancesMetrics thisServer
             , "RelayStatsE"                                 : RelayHandler.stats
             , "IngestAggregatorE"                           : IngestAggregatorHandler.ingestAggregator
             , "IngestAggregatorPlayerE"                     : \(_ :: SlotId) (_ :: SlotRole) -> PrivFile "rtsv2" "www/aggregatorPlayer.html"
             , "IngestAggregatorPlayerJsE"                   : \(_ :: SlotId) (_ :: SlotRole) -> PrivDir "rtsv2" "www/assets/js"
             , "IngestAggregatorActiveIngestsPlayerE"        : \(_ :: SlotId) (_ :: SlotRole) (_ :: ProfileName) -> PrivFile "rtsv2" "www/play.html"
             , "IngestAggregatorActiveIngestsPlayerJsE"      : \(_ :: SlotId) (_ :: SlotRole) (_ :: ProfileName) -> PrivDir "rtsv2" "www/assets/js"
             , "IngestAggregatorActiveIngestsPlayerControlE" : CowboyRoutePlaceholder
             , "IngestInstancesMetricsE"                     : IngestHandler.ingestInstancesMetrics
             , "IngestInstanceE"                             : IngestHandler.ingestInstance
             , "ClientAppAssetsE"                            : PrivDir Config.appName "www/assets"
             , "ClientAppRouteHTMLE"                         : PrivFile Config.appName "www/index.html"

             , "CanaryStreamDiscoveryE"                      : if canaryState == Canary
                                                               then StreamDiscoveryHandler.discover loadConfig Canary
                                                               else \(_ :: RtmpShortName) (_ :: SlotName) -> ServiceUnavailable.unavailable
             , "CanaryClientPlayerE"                         : \(_ :: SlotId) (_ :: SlotRole) -> PrivFile "rtsv2" "www/egestReferencePlayer.html"
             , "CanaryClientPlayerAssetsE"                   :  \(_ :: SlotId) (_ :: SlotRole) -> PrivDir "rtsv2" "www/assets"
             , "CanaryClientPlayerControlE"                  : CowboyRoutePlaceholder
             , "CanaryClientWebRTCIngestE"                   : \(_ :: RtmpShortName) (_ :: RtmpStreamName) -> PrivFile "rtsv2" "www/ingestReferencePlayer.html"
             , "CanaryClientWebRTCIngestAssetsE"             : \(_ :: RtmpShortName) (_ :: RtmpStreamName) -> PrivDir "rtsv2" "www/assets"
             , "CanaryClientWebRTCIngestControlE"            : CowboyRoutePlaceholder
             , "FaviconE"                                    : PrivFile "rtsv2" "www/assets/images/favicon.ico"
             }
         # Stetson.cowboyRoutes supportCowboyRoutes
         # Stetson.port webConfig.supportPort
         # (uncurry4 Stetson.bindTo) (ipToTuple supportBindIp)
         # Stetson.startClear "support_http"
  where
    supportCowboyRoutes :: List Path
    supportCowboyRoutes =
      -- Some duplication of URLs here from those in Endpoint.purs due to current inability to build cowboy-style bindings from stongly-typed parameters

      -- IngestAggregatorActiveIngestsPlayerControlE SlotId SlotRole ProfileName
      cowboyRoute ("/support/ingestAggregator/" <> slotIdBinding <> "/" <> slotRoleBinding <> "/activeIngests/" <> profileNameBinding <> "/control")
                  "rtsv2_player_ws_resource"
                  (unsafeToForeign { mode: (atom "ingest")
                                   , make_ingest_key: makeIngestKey
                                   , use_media_gateway:
                                     case mediaGateway of
                                       Off ->
                                         false
                                       _ ->
                                         true
                                   })

      -- CanaryClientPlayerControlE SlotId SlotRole
      : cowboyRoute ("/support/canary/client/" <> slotIdBinding <> "/" <> slotRoleBinding <> "/session")
                    (if canaryState == Canary then "rtsv2_player_ws_resource" else "service_unavailable")
                    (playerControlArgs loadConfig webConfig mediaGateway Canary validationUrlWhitelist)

      -- CanaryClientWebRTCIngestContorlE SlotId SlotRole
      : cowboyRoute ("/support/canary/ingest/" <> accountBinding <> "/" <> streamNameBinding <> "/session")
                    (if canaryState == Canary then "rtsv2_webrtc_push_ingest_ws_resource" else "service_unavailable")
                    (ingestControlArgs thisServer loadConfig Canary)

      : nil

systemRoutes ::  WebConfig -> Config.LoadConfig -> Config.FeatureFlags -> Server -> Effect Unit
systemRoutes webConfig loadConfig featureFlags@{mediaGateway} thisServer = do
  systemBindIp <- Env.systemListenIp
  void $ Stetson.configure
       # Stetson.routes
           System.endpoint
           { "TransPoPLeaderE"                             : IntraPoPHandler.leader
           , "EgestE"                                      : EgestHandler.startResource loadConfig
           , "RelayE"                                      : RelayHandler.startResource loadConfig
           , "RelayEnsureStartedE"                         : RelayHandler.ensureStarted loadConfig
           , "RelayRegisteredRelayWs"                      : RelayHandler.registeredRelayWs
           , "RelayRegisteredEgestWs"                      : RelayHandler.registeredEgestWs

           , "IngestAggregatorsE"                          : IngestAggregatorHandler.ingestAggregators loadConfig
           , "IngestAggregatorRegisteredIngestWs"          : IngestAggregatorHandler.registeredIngestWs
           , "IngestAggregatorRegisteredRelayWs"           : IngestAggregatorHandler.registeredRelayWs
           , "IngestAggregatorBackupWs"                    : IngestAggregatorHandler.backupWs

           , "IngestInstanceLlwpE"                         : CowboyRoutePlaceholder
           , "IntraPoPTestHelperE"                         : IntraPoPHandler.testHelper
           , "LoadE"                                       : LoadHandler.load
           , "RelayProxiedStatsE"                          : RelayHandler.proxiedStats

           , "IngestStartE"                                : IngestHandler.ingestStart loadConfig
           , "IngestStopE"                                 : IngestHandler.ingestStop
           , "ClientStartE"                                : ClientHandler.clientStart loadConfig
           , "ClientStopE"                                 : ClientHandler.clientStop
           , "Chaos"                                       : ChaosHandler.chaos

           , "StreamAuthTypeE"                             : LlnwStubHandler.streamAuthType
           , "StreamAuthE"                                 : LlnwStubHandler.streamAuth
           , "StreamPublishE"                              : LlnwStubHandler.streamPublish
           , "SlotLookupE"                                 : LlnwStubHandler.slotLookup
           , "HlsPushE"                                    : LlnwStubHandler.hlsPush
           , "ValidationE"                                 : LlnwStubHandler.validation

           , "WorkflowsE"                                  : CowboyRoutePlaceholder -- id3as_workflows_resource
           , "WorkflowGraphE"                              : CowboyRoutePlaceholder -- id3as_workflow_graph_resource
           , "WorkflowMetricsE"                            : CowboyRoutePlaceholder --id3as_workflow_graph_resource
           , "WorkflowStructureE"                          : CowboyRoutePlaceholder -- id3as_workflow_graph_resource
           , "WorkflowConfigE"                             : CowboyRoutePlaceholder -- rtsv2_workflow_config_resource
           , "VisualiserE"                                 : \(_ :: String) -> PrivFile "id3as_media" "www/visualiser.html"
           , "VisualiserAssetsE"                           : \(_ :: String) -> PrivDir "id3as_media" "www"
           , "FaviconE"                                    : PrivFile "rtsv2" "www/assets/images/favicon.ico"
           }
       # Stetson.cowboyRoutes systemCowboyRoutes
       # Stetson.port webConfig.systemPort
       # (uncurry4 Stetson.bindTo) (ipToTuple systemBindIp)
       # Stetson.startClear "system_http"
  where
    systemCowboyRoutes :: List Path
    systemCowboyRoutes =
      -- Some duplication of URLs here from those in Endpoint.purs due to current inability to build cowboy-style bindings from stongly-typed parameters
        cowboyRoute ("/system/ingest/" <> slotIdBinding <> "/" <> slotRoleBinding <> "/" <> profileNameBinding <> "/llwp")
                   "llwp_stream_resource"
                   ((unsafeToForeign) IngestKey)

      --workflows
      : cowboyRoute ("/system/workflows") "id3as_workflows_resource" (unsafeToForeign unit)
      -- WorkflowGraphE String
      : cowboyRoute ("/system/workflows/" <> referenceBinding <> "/graph") "id3as_workflow_graph_resource" (unsafeToForeign (atom "graph"))
      -- WorkflowMetricsE String
      : cowboyRoute ("/system/workflows/" <> referenceBinding <> "/metrics") "id3as_workflow_graph_resource" (unsafeToForeign (atom "metrics"))
      -- WorkflowStructureE String
      : cowboyRoute ("/system/workflows/" <> referenceBinding <> "/structure") "id3as_workflow_graph_resource" (unsafeToForeign (atom "structure"))

      -- workflow/config is hardcoded visualiser path
      : cowboyRoute ("/system/workflows/" <> referenceBinding <> "/workflow/config") "rtsv2_workflow_config_resource" (unsafeToForeign nil)

      : nil


playerControlArgs :: Config.LoadConfig -> WebConfig -> MediaGatewayFlag -> CanaryState -> List String -> Foreign
playerControlArgs loadConfig webConfig mediaGateway canary validationUrlWhitelist =
  unsafeToForeign { mode: (atom "egest")
                  , canary
                  , make_egest_key: makeEgestKey
                  , start_stream: (\(EgestKey slotId slotRole) -> EgestInstanceSup.findEgest loadConfig canary slotId slotRole)
                  , add_client: mkFn3 EgestInstance.addClient
                  , get_slot_configuration: EgestInstance.getSlotConfiguration
                  , data_object_send_message: EgestInstance.dataObjectSendMessage
                  , data_object_update: EgestInstance.dataObjectUpdate
                  , stats_update: mkFn2 EgestInstance.statsUpdate
                  , public_port: webConfig.publicPort
                  , support_port: webConfig.supportPort
                  , validation_url_whitelist: validationUrlWhitelist
                  , use_media_gateway:
                    case mediaGateway of
                      Off -> false
                      _ -> true
                  }

ingestControlArgs :: Server -> Config.LoadConfig -> CanaryState -> Foreign
ingestControlArgs thisServer loadConfig canary =
  unsafeToForeign { authenticate: mkFn7 $ IngestWebRTCIngestHandler.authenticate loadConfig canary (unwrap $ extractAddress thisServer)
                  }

makeEgestKey :: UUID -> SlotRole -> EgestKey
makeEgestKey slotId slotRole =
  EgestKey (wrap slotId) slotRole

makeIngestKey :: UUID -> String -> String -> IngestKey
makeIngestKey slotId slotRole profileName =
  IngestKey (wrap slotId) (parseSlotRole slotRole) (wrap profileName)
  where
    parseSlotRole "primary" = Primary
    parseSlotRole "backup" = Backup
    parseSlotRole _ = Primary

cowboyRoute :: String -> String -> Foreign -> Path
cowboyRoute path moduleName initialState =
  Path (tuple3
        (matchSpec path)
        (NativeModuleName $ atom moduleName)
        (InitialState $ initialState)
       )

slotIdBinding :: String
slotIdBinding = ":slot_id"

slotRoleBinding :: String
slotRoleBinding = ":slot_role"

accountBinding :: String
accountBinding = ":account"

streamNameBinding :: String
streamNameBinding = ":stream_name"

profileNameBinding :: String
profileNameBinding = ":profile_name"

referenceBinding :: String
referenceBinding = ":reference"

ipToTuple :: Ip -> Tuple4 Int Int Int Int
ipToTuple (Ipv4 a b c d) = tuple4 a b c d

emptyText  :: forall a. Tuple2 String (Req -> a -> (Effect (RestResult String a)))
emptyText = textWriter ""

textWriter :: forall a. String -> Tuple2 String (Req -> a -> (Effect (RestResult String a)))
textWriter text = tuple2 "text/plain" (\req state -> Rest.result text req state)


--------------------------------------------------------------------------------
-- Log helpers
--------------------------------------------------------------------------------
domain :: List Atom
domain = serverName # Names.toDomain # singleton

logInfo :: forall report. String -> { | report } -> Effect Unit
logInfo = Logger.info <<< Logger.traceMetadata domain
