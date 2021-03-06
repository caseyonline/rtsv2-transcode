module Rtsv2.Config
  (
    AlertConfig
  , EgestAgentConfig
  , EgestStatsConfig
  , FeatureFlags
  , GlobalConfig
  , HealthConfig
  , IngestAggregatorAgentConfig
  , IngestInstanceConfig
  , IngestStatsConfig
  , IntraPoPAgentApi
  , IntraPoPAgentConfig
  , LlnwApiConfig
  , LoadConfig
  , MediaGatewayFlag(..)
  , NodeManagerConfig
  , PoPDefinitionConfig
  , StreamRelayConfig
  , TransPoPAgentApi
  , TransPoPAgentConfig
  , alertConfig
  , appName
  , egestAgentConfig
  , egestStatsConfig
  , featureFlags
  , globalConfig
  , healthConfig
  , ingestAggregatorAgentConfig
  , ingestInstanceConfig
  , ingestStatsConfig
  , intraPoPAgentConfig
  , llnwApiConfig
  , loadConfig
  , mergeOverrides
  , nodeManagerConfig
  , popDefinitionConfig
  , rtmpIngestConfig
  , rtmpEgestConfig
  , streamRelayConfig
  , transPoPAgentConfig
  , webConfig
  ) where

import Prelude

import Control.Monad.Except (ExceptT, except, runExcept)
import Data.Either (Either(..), hush, note)
import Data.Identity (Identity)
import Data.List.NonEmpty (singleton)
import Data.Maybe (Maybe(..), fromMaybe, fromMaybe')
import Data.Traversable (traverse)
import Effect (Effect)
import Erl.Atom (Atom, atom)
import Erl.Data.List (List, nil, (:))
import Erl.Data.Tuple (Tuple2, tuple2)
import Erl.Utils (base64Encode)
import Foreign (F, Foreign, ForeignError(..), readString, unsafeReadTagged)
import Logger as Logger
import Partial.Unsafe (unsafeCrashWith)
import Rtsv2.LoadTypes as LoadTypes
import Shared.Rtsv2.Agent (Agent, SlotCharacteristics, strToAgent)
import Shared.Rtsv2.Stream (AgentKey)
import Shared.Rtsv2.Types (CanaryState, RunState, Server, WebConfig)
import Shared.Utils (lazyCrashIfMissing)
import Simple.JSON (class ReadForeign, readImpl)


type LoadConfig
  = { loadAnnounceMs :: Int
    , monitorLoad :: Boolean
    , limits :: LoadTypes.LoadLimits
    , costs :: LoadTypes.LoadCosts
    }

type AlertConfig
  = { alertRetentionMs :: Int
    }

type NodeManagerConfig
  = { initialRunState :: RunState
    , initialCanaryState :: CanaryState
    , forceDrainTimeoutMs :: Int
    }

type FeatureFlags
  = { mediaGateway :: MediaGatewayFlag
    }

data MediaGatewayFlag
  = Off
  | On
  | External

instance readForeignMediaGatewayFlag :: ReadForeign MediaGatewayFlag where
  readImpl =
    readString >=> parse
    where
      error s = singleton (ForeignError (errorString s))
      parse s = except $ note (error s) (toType s)
      toType "off" = pure Off
      toType "on" = pure On
      toType "external" = pure External
      toType unknown = Nothing
      errorString s = "Unknown Media Gateway Flag: " <> s

type GlobalConfig
  = { intraPoPLatencyMs :: Int
    }

type PoPDefinitionConfig
  = { directory :: String
    , popDefinitionFile :: String
    , wanDefinitionFile :: String
    }

type IngestInstanceConfig
  = { eqLogIntervalMs :: Int
    , aggregatorRetryTimeMs :: Int
    , qosPollIntervalMs :: Int
    , abortIfNoMediaMs :: Int
    , qosAverageBitrateLowWatermark :: Number
    , qosAverageBitrateHighWatermark :: Number
    , qosPeakBitrateLowWatermark :: Number
    , qosPeakBitrateHighWatermark :: Number
    }

type IngestAggregatorAgentConfig
  = { shutdownLingerTimeMs :: Int
    , forceDrainTimeoutMs :: Int
    }

type IngestStatsConfig
  = { pollPeriodMs :: Int
    }

type EgestStatsConfig
  = { pollPeriodMs :: Int
    }

type EgestAgentConfig
  = { eqLogIntervalMs :: Int
    , lingerTimeMs :: Int
    , relayCreationRetryMs :: Int
    , reserveForPotentialNumClients :: Int
    , decayReserveMs :: Int
    , forceDrainTimeoutMs :: Int
    , numForceDrainPhases :: Int
    , aggregatorExitLingerTimeMs :: Int
    , clientCountReportingTimeMs :: Int
    , maxMessageSize :: Int
    , maxMessagesPerSecondPerClient :: Number
    , maxMessagesPerSecondPerSlot :: Number
    }

type IntraPoPAgentConfig
  = { bindPort :: Int
    , rpcPort :: Int
    , rejoinEveryMs :: Int
    , replayMessagesOnJoin :: Boolean
    , vmLivenessIntervalMs :: Int
    , reannounceAgentEveryMs :: { aggregator :: Int
                                , relay :: Int
                                , egest :: Int
                                }
    , missCountBeforeExpiry :: Int
    }

type TransPoPAgentConfig
  = { bindPort :: Int
    , rpcPort :: Int
    , leaderTimeoutMs :: Int
    , leaderAnnounceMs :: Int
    , rttRefreshMs :: Int
    , rejoinEveryMs :: Int
    , defaultRttMs :: Int
    , connectStreamAfterMs :: Int
    , replayMessagesOnJoin :: Boolean
    }

type StreamRelayConfig
  = { lingerTimeMs :: Int
    , reApplyPlanTimeMs :: Int
    , forceDrainTimeoutMs :: Int
    , aggregatorExitLingerTimeMs :: Int
    , clientCountReportingTimeMs :: Int
    }

type IntraPoPAgentApi
  = { announceOtherPoPAggregatorIsAvailable :: SlotCharacteristics -> AgentKey -> Server -> Effect Unit
    , announceOtherPoPAggregatorStopped :: AgentKey -> Server -> Effect Unit
    , announceTransPoPLeader :: Effect Unit
    }

type TransPoPAgentApi
  = { announceAggregatorIsAvailable :: SlotCharacteristics -> AgentKey -> Server -> Effect Unit
    , announceAggregatorStopped :: AgentKey -> Server -> Effect Unit
    , handleRemoteLeaderAnnouncement :: Server -> Effect Unit
    }

type RtmpIngestConfig
  = { port :: Int
    , canaryPort :: Int
    , tlsPort :: Int
    , canaryTlsPort :: Int
    , certFile :: String
    , keyFile :: String
    , canaryCertFile :: String
    , canaryKeyFile :: String
    , nbAcceptors :: Int
    , cryptoContextExpiryMs :: Int
    }

type RtmpEgestConfig
  = { port :: Int
    , canaryPort :: Int
    , tlsPort :: Int
    , canaryTlsPort :: Int
    , certFile :: String
    , keyFile :: String
    , canaryCertFile :: String
    , canaryKeyFile :: String
    , nbAcceptors :: Int
    }

type LlnwApiConfig
  = { streamAuthTypeUrl :: String
    , streamAuthUrl :: String
    , streamPublishUrl :: String
    , slotLookupUrl :: String
    , headers :: List (Tuple2 String String)
    , defaultSegmentDurationMs :: Int
    , defaultPlaylistDurationMs :: Int
    , validationUrlWhitelist :: List String
    , slotLookupExpiryTimeMs :: Int
    }

type LlnwApiConfigInternal
  = { streamAuthTypeUrl :: String
    , streamAuthUrl :: String
    , streamPublishUrl :: String
    , slotLookupUrl :: String
    , useBasicAuth :: Maybe String
    , defaultSegmentDurationMs :: Int
    , defaultPlaylistDurationMs :: Int
    , validationUrlWhitelist :: List String
    , slotLookupExpiryTimeMs :: Int
    }

type HealthConfig
  = { thresholds :: { perfect :: Int
                    , excellent :: Int
                    , good :: Int
                    , poor :: Int
                      }
    }


foreign import getEnv_ :: Atom -> Effect Foreign
foreign import getMap_ :: Atom -> Effect Foreign
foreign import mergeOverrides :: Effect Foreign

appName :: String
appName = "rtsv2"

loadConfig :: Effect LoadConfig
loadConfig = do
  getMandatoryRecord "loadConfig"

alertConfig :: Effect AlertConfig
alertConfig = do
  getMandatoryRecord "alertConfig"

nodeManagerConfig :: Effect NodeManagerConfig
nodeManagerConfig = do
  getMandatoryRecord "nodeManagerConfig"

featureFlags :: Effect FeatureFlags
featureFlags = do
  getMandatoryRecord "featureFlags"

webConfig :: Effect WebConfig
webConfig = do
  config <- getMandatory (unsafeReadTagged "map") "httpApiConfig"
  pure $ config

globalConfig :: Effect GlobalConfig
globalConfig = do
  config <- getMandatory (unsafeReadTagged "map") "globalConfig"
  pure $ config

configuredAgents :: Effect (List Agent)
configuredAgents = do
  agentStrings <- getMandatory (readList >=> traverse readString) "agents"
  pure $ strToAgent <$> agentStrings

popDefinitionConfig :: Effect PoPDefinitionConfig
popDefinitionConfig = do
  config <- getMandatory (unsafeReadTagged "map") "popDefinitionConfig"
  pure $ config

intraPoPAgentConfig :: Effect IntraPoPAgentConfig
intraPoPAgentConfig = do
  getMandatoryRecord "intraPoPConfig"

transPoPAgentConfig :: Effect TransPoPAgentConfig
transPoPAgentConfig = do
  getMandatoryRecord "transPoPConfig"

ingestAggregatorAgentConfig :: Effect IngestAggregatorAgentConfig
ingestAggregatorAgentConfig = do
  getMandatoryRecord "ingestAggregatorConfig"

streamRelayConfig :: Effect StreamRelayConfig
streamRelayConfig = do
  getMandatoryRecord "streamRelayConfig"

ingestStatsConfig :: Effect IngestStatsConfig
ingestStatsConfig = do
  getMandatoryRecord "ingestStatsConfig"

egestStatsConfig :: Effect EgestStatsConfig
egestStatsConfig = do
  getMandatoryRecord "egestStatsConfig"

ingestInstanceConfig :: Effect IngestInstanceConfig
ingestInstanceConfig = do
  getMandatoryRecord "ingestInstanceConfig"

egestAgentConfig :: Effect EgestAgentConfig
egestAgentConfig = do
  getMandatoryRecord "egestConfig"

rtmpIngestConfig :: Effect RtmpIngestConfig
rtmpIngestConfig = do
  getMandatoryRecord "rtmpIngestConfig"

rtmpEgestConfig :: Effect RtmpEgestConfig
rtmpEgestConfig = do
  getMandatoryRecord "rtmpEgestConfig"


llnwApiConfig :: Effect LlnwApiConfig
llnwApiConfig = do
  (internal :: LlnwApiConfigInternal) <- getMandatoryRecord "llnwApiConfig"
  let
    { streamAuthTypeUrl
    , streamAuthUrl
    , streamPublishUrl
    , slotLookupUrl
    , useBasicAuth
    , defaultSegmentDurationMs
    , defaultPlaylistDurationMs
    , validationUrlWhitelist
    , slotLookupExpiryTimeMs
    } = internal

    external = { streamAuthTypeUrl
               , streamAuthUrl
               , streamPublishUrl
               , slotLookupUrl
               , headers : case useBasicAuth of
                             Nothing -> nil
                             Just auth ->
                               let headerValue = "Basic " <> base64Encode auth
                               in (tuple2 "Authorization" headerValue) : nil
               , defaultSegmentDurationMs
               , defaultPlaylistDurationMs
               , validationUrlWhitelist
               , slotLookupExpiryTimeMs
               }
  pure external

healthConfig :: Effect HealthConfig
healthConfig = do
  getMandatoryRecord "healthConfig"

get :: forall a e. (Foreign -> ExceptT e Identity a) -> String -> Effect (Maybe a)
get f v = hush <<< runExcept <<< f <$> getEnv_ (atom v)

getMandatory :: forall a e. (Foreign -> ExceptT e Identity a) -> String -> Effect a
getMandatory f v = fromMaybe' (lazyCrashIfMissing $ "Missing mandatory config value " <> v) <$> hush <<< runExcept <<< f <$> getEnv_ (atom v)

getWithDefault :: forall a e. (Foreign -> ExceptT e Identity a) -> String -> a -> Effect a
getWithDefault f v d = fromMaybe d <$> (get f v)

-- TODO: Rehome with foreign or list
readList :: Foreign -> F (List Foreign)
readList = unsafeReadTagged "list"

getMandatoryRecord :: forall a. ReadForeign a => String -> Effect a
getMandatoryRecord v = do
  map <- getMap_ (atom v)
  case runExcept $ readImpl map of
    Left error -> do
      Logger.error { domain: (atom "config") : nil
                   , type: Logger.Trace
                   }
                   { text: "Invalid Config"
                   , key: v
                   , error: error
                   }
      unsafeCrashWith ("invalid_config " <> v)
    Right ok ->
      pure $ ok
