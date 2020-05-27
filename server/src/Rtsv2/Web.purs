module Rtsv2.Web
  ( startLink
  )
  where

import Prelude

import Data.Function.Uncurried (mkFn2, mkFn3, mkFn7)
import Data.Maybe (fromMaybe)
import Data.Newtype (unwrap, wrap)
import Effect (Effect)
import Erl.Atom (Atom, atom)
import Erl.Cowboy.Req (Req)
import Erl.Cowboy.Routes (InitialState(..), Path(..), matchSpec)
import Erl.Data.List (List, nil, singleton, (:))
import Erl.Data.Tuple (Tuple2, Tuple4, tuple2, tuple3, tuple4, uncurry4)
import Erl.ModuleName (NativeModuleName(..))
import Erl.Process.Raw (Pid)
import Foreign (unsafeToForeign)
import Logger (Logger)
import Logger as Logger
import Pinto (ServerName, StartLinkResult)
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
import Rtsv2.Handler.StreamDiscovery as StreamDiscoveryHandler
import Rtsv2.Handler.TransPoP as TransPoPHandler
import Rtsv2.Names as Names
import Rtsv2.PoPDefinition as PoPDefinition
import Rtsv2.Types (LocationResp, RegistrationResp)
import Serf (Ip(..))
import Shared.Rtsv2.LlnwApiTypes (StreamDetails)
import Shared.Rtsv2.Router.Endpoint.Public as Public
import Shared.Rtsv2.Router.Endpoint.Support as Support
import Shared.Rtsv2.Router.Endpoint.System as System
import Shared.Rtsv2.Stream (EgestKey(..), IngestKey(..), ProfileName, RtmpShortName, RtmpStreamName, SlotId, SlotRole(..))
import Shared.Rtsv2.Types (CanaryState(..), Server, WebConfig, extractAddress)
import Shared.UUID (UUID, fromString)
import Shared.UUID as UUID
import Stetson (RestResult, StaticAssetLocation(..))
import Stetson as Stetson
import Stetson.Rest as Rest
import Stetson.Types (CowboyRoutePlaceholder(..))

newtype State = State {}

serverName :: ServerName State Unit
serverName = Names.webServerName

startLink :: WebConfig -> Effect StartLinkResult
startLink args =
  Gen.startLink serverName (init args) Gen.defaultHandleInfo

init :: WebConfig -> Effect State
init args = do
  featureFlags <- Config.featureFlags
  loadConfig <- Config.loadConfig
  publicBindIp <- Env.publicInterfaceIp
  privateBindIp <- Env.privateInterfaceIp
  thisServer <- PoPDefinition.getThisServer

  public <-
        Stetson.configure
      # Stetson.routes
          Public.endpoint
          { "StreamDiscoveryE"                            : StreamDiscoveryHandler.discover loadConfig Live
          , "ClientPlayerE"                               : \(_ :: SlotId) (_ :: SlotRole) -> PrivFile "rtsv2" "www/egestReferencePlayer.html"
          , "ClientPlayerAssetsE"                         : \(_ :: SlotId) (_ :: SlotRole) -> PrivDir "rtsv2" "www/assets"
          , "ClientPlayerControlE"                        : CowboyRoutePlaceholder

          , "ClientWebRTCIngestE"                         : \(_ :: RtmpShortName) (_ :: RtmpStreamName) -> PrivFile "rtsv2" "www/referenceIngest.html"
          , "ClientWebRTCIngestAssetsE"                   : \(_ :: RtmpShortName) (_ :: RtmpStreamName) -> PrivDir "rtsv2" "www/assets"
          , "ClientWebRTCIngestControlE"                  : CowboyRoutePlaceholder
          }
      # Stetson.cowboyRoutes (publicRoutes thisServer featureFlags loadConfig)
      # Stetson.port args.publicPort
      # (uncurry4 Stetson.bindTo) (ipToTuple publicBindIp)
      # Stetson.startClear "public_http"

  support <-
     Stetson.configure
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
          , "IngestAggregatorsE"                          : IngestAggregatorHandler.ingestAggregators loadConfig
          , "IngestInstancesMetricsE"                     : IngestHandler.ingestInstancesMetrics
          , "IngestInstanceE"                             : IngestHandler.ingestInstance
          , "ClientAppAssetsE"                            : PrivDir Config.appName "www/assets"
          , "ClientAppRouteHTMLE"                         : PrivFile Config.appName "www/index.html"

          , "CanaryStreamDiscoveryE"                      : StreamDiscoveryHandler.discover loadConfig Canary
          , "CanaryClientPlayerE"                         : \(_ :: SlotId) (_ :: SlotRole) -> PrivFile "rtsv2" "www/egestReferencePlayer.html"
          , "CanaryClientPlayerAssetsE"                   : \(_ :: SlotId) (_ :: SlotRole) -> PrivDir "rtsv2" "www/assets"
          , "CanaryClientPlayerControlE"                  : CowboyRoutePlaceholder

          , "CanaryClientWebRTCIngestE"                   : \(_ :: RtmpShortName) (_ :: RtmpStreamName) -> PrivFile "rtsv2" "www/referenceIngest.html"
          , "CanaryClientWebRTCIngestAssetsE"             : \(_ :: RtmpShortName) (_ :: RtmpStreamName) -> PrivDir "rtsv2" "www/assets"
          , "CanaryClientWebRTCIngestControlE"            : CowboyRoutePlaceholder

          }
      # Stetson.cowboyRoutes (supportRoutes thisServer featureFlags loadConfig)
      # Stetson.port args.supportPort
      # (uncurry4 Stetson.bindTo) (ipToTuple privateBindIp)
      # Stetson.startClear "support_http"

  system <-
    Stetson.configure
      # Stetson.routes
          System.endpoint
          { "TransPoPLeaderE"                             : IntraPoPHandler.leader
          , "EgestE"                                      : EgestHandler.startResource loadConfig
          , "RelayE"                                      : RelayHandler.startResource loadConfig
          , "RelayEnsureStartedE"                         : RelayHandler.ensureStarted loadConfig
          , "RelayRegisteredRelayWs"                      : RelayHandler.registeredRelayWs
          , "RelayRegisteredEgestWs"                      : RelayHandler.registeredEgestWs

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

          , "WorkflowsE"                                  : CowboyRoutePlaceholder -- id3as_workflows_resource
          , "WorkflowGraphE"                              : CowboyRoutePlaceholder -- id3as_workflow_graph_resource
          , "WorkflowMetricsE"                            : CowboyRoutePlaceholder --id3as_workflow_graph_resource
          , "WorkflowStructureE"                          : CowboyRoutePlaceholder -- id3as_workflow_graph_resource
          }
      # Stetson.cowboyRoutes (systemRoutes thisServer featureFlags loadConfig)
      # Stetson.port args.systemPort
      # (uncurry4 Stetson.bindTo) (ipToTuple privateBindIp)
      # Stetson.startClear "system_http"

  pure $ State {}

  where

    supportRoutes :: Server -> Config.FeatureFlags -> Config.LoadConfig -> List Path
    supportRoutes thisServer { mediaGateway } loadConfig =
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

      -- ClientPlayerControlE Canary SlotId
      : cowboyRoute ("/public/client/" <> slotIdBinding <> "/" <> slotRoleBinding <> "/session")
                    "rtsv2_player_ws_resource"
                    (playerControlArgs loadConfig mediaGateway Live)

      -- ClientWebRTCIngestContorlE SlotId SlotRole
      : cowboyRoute ("/public/ingest/" <> accountBinding <> "/" <> streamNameBinding <> "/session")
                    "rtsv2_webrtc_push_ingest_ws_resource"
                    (ingestControlArgs thisServer loadConfig Live)

      -- CanaryClientPlayerControlE SlotId SlotRole
      : cowboyRoute ("/support/canary/client/" <> slotIdBinding <> "/" <> slotRoleBinding <> "/session")
                    "rtsv2_player_ws_resource"
                    (playerControlArgs loadConfig mediaGateway Canary)

      -- CanaryClientWebRTCIngestContorlE SlotId SlotRole
      : cowboyRoute ("/suport/canary/ingest/" <> accountBinding <> "/" <> streamNameBinding <> "/session")
                    "rtsv2_webrtc_push_ingest_ws_resource"
                    (ingestControlArgs thisServer loadConfig Canary)

      : nil

    systemRoutes :: Server -> Config.FeatureFlags -> Config.LoadConfig -> List Path
    systemRoutes thisServer { mediaGateway } loadConfig =
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

      : nil

    publicRoutes :: Server -> Config.FeatureFlags -> Config.LoadConfig -> List Path
    publicRoutes thisServer { mediaGateway } loadConfig =
      -- Some duplication of URLs here from those in Endpoint.purs due to current inability to build cowboy-style bindings from stongly-typed parameters
      -- ClientPlayerControlE Canary SlotId
        cowboyRoute ("/public/client/" <> slotIdBinding <> "/" <> slotRoleBinding <> "/session")
                    "rtsv2_player_ws_resource"
                    (playerControlArgs loadConfig mediaGateway Live)

      -- ClientWebRTCIngestContorlE SlotId SlotRole
      : cowboyRoute ("/public/ingest/" <> accountBinding <> "/" <> streamNameBinding <> "/session")
                    "rtsv2_webrtc_push_ingest_ws_resource"
                    (ingestControlArgs thisServer loadConfig Live)

      : nil

    playerControlArgs loadConfig mediaGateway canary =
      unsafeToForeign { mode: (atom "egest")
                      , canary
                      , make_egest_key: makeEgestKey
                      , start_stream: startStream loadConfig canary
                      , add_client: mkFn3 addClient
                      , get_slot_configuration: EgestInstance.getSlotConfiguration
                      , data_object_send_message: EgestInstance.dataObjectSendMessage
                      , data_object_update: EgestInstance.dataObjectUpdate
                      , stats_update: mkFn2 EgestInstance.statsUpdate
                      , use_media_gateway:
                        case mediaGateway of
                          Off -> false
                          _ -> true
                      }

    ingestControlArgs thisServer loadConfig canary =
      unsafeToForeign { authenticate: mkFn7 $ IngestWebRTCIngestHandler.authenticate loadConfig canary (unwrap $ extractAddress thisServer)
                      }

    slotIdBinding = ":slot_id"
    slotRoleBinding = ":slot_role"
    profileNameBinding = ":profile_name"
    serverAddressBinding = ":server_address"
    portBinding = ":port"
    sourceRouteBinding = ":source_route"
    canaryBinding = ":canary"
    referenceBinding = ":reference"
    accountBinding = ":account"
    streamNameBinding = ":stream_name"

    makeIngestKey2 :: StreamDetails -> ProfileName -> IngestKey
    makeIngestKey2 { slot : { id }, role } profileName =
      IngestKey id role profileName

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

    slotIdStringToSlotId :: String -> SlotId
    slotIdStringToSlotId slotIdStr =
      wrap $ fromMaybe UUID.empty (fromString slotIdStr)

    startStream :: Config.LoadConfig -> CanaryState -> EgestKey -> Effect LocationResp
    startStream loadConfig canary (EgestKey slotId slotRole) =
        EgestInstanceSup.findEgest loadConfig canary slotId slotRole

    addClient :: Pid -> EgestKey -> String -> Effect RegistrationResp
    addClient pid egestKey sessionId =
      EgestInstance.addClient pid egestKey sessionId

    cowboyRoute path moduleName initialState =
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

logInfo :: forall a. Logger (Record a)
logInfo = domainLog Logger.info

--logWarning :: forall a. Logger a
--logWarning = domainLog Logger.warning

domainLog :: forall a. Logger {domain :: List Atom, misc :: Record a} -> Logger (Record a)
domainLog = Logger.doLog domains
