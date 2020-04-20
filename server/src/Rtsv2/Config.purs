module Rtsv2.Config
  ( FeatureFlags
  , GlobalConfig
  , IngestInstanceConfig
  , IngestAggregatorAgentConfig
  , IngestStatsConfig
  , WebConfig
  , PoPDefinitionConfig
  , IntraPoPAgentConfig
  , TransPoPAgentConfig
  , StreamRelayConfig
  , EgestAgentConfig
  , IntraPoPAgentApi
  , TransPoPAgentApi
  , LlnwApiConfig
  , LoadMonitorConfig
  , HealthConfig
  , appName
  , featureFlags
  , webConfig
  , globalConfig
  , nodeConfig
  , popDefinitionConfig
  , intraPoPAgentConfig
  , transPoPAgentConfig
  , ingestStatsConfig
  , ingestInstanceConfig
  , ingestAggregatorAgentConfig
  , streamRelayConfig
  , egestAgentConfig
  , rtmpIngestConfig
  , llnwApiConfig
  , loadMonitorConfig
  , healthConfig
  , mergeOverrides
  ) where

import Prelude

import Control.Monad.Except (ExceptT, runExcept)
import Data.Either (Either(..), hush)
import Data.Identity (Identity)
import Data.Maybe (Maybe, fromMaybe, fromMaybe')
import Data.Traversable (traverse)
import Effect (Effect)
import Erl.Atom (Atom, atom)
import Erl.Data.List (List)
import Foreign (F, Foreign, readString, unsafeReadTagged)
import Logger as Logger
import Partial.Unsafe (unsafeCrashWith)
import Rtsv2.Node as Node
import Shared.Agent (Agent, strToAgent)
import Shared.Stream (AgentKey)
import Shared.Types (Server)
import Shared.Utils (lazyCrashIfMissing)
import Simple.JSON (class ReadForeign, readImpl)

type FeatureFlags
  = { useMediaGateway :: Boolean
    }

-- TODO - config should include BindIFace or BindIp
type WebConfig = { port :: Int }

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
    }

type IngestAggregatorAgentConfig
  = { shutdownLingerTimeMs :: Int
    }

type IngestStatsConfig
  = { pollPeriodMs :: Int
    }

type EgestAgentConfig
  = { eqLogIntervalMs :: Int
    , lingerTimeMs :: Int
    , relayCreationRetryMs :: Int
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
    }

type IntraPoPAgentApi
  = { announceOtherPoPAggregatorIsAvailable :: AgentKey -> Server -> Effect Unit
    , announceOtherPoPAggregatorStopped :: AgentKey -> Server -> Effect Unit
    , announceTransPoPLeader :: Effect Unit
    }

type TransPoPAgentApi
  = { announceAggregatorIsAvailable :: AgentKey -> Server -> Effect Unit
    , announceAggregatorStopped :: AgentKey -> Server -> Effect Unit
    , handleRemoteLeaderAnnouncement :: Server -> Effect Unit
    }

type RtmpIngestConfig
  = { port :: Int
    , nbAcceptors :: Int
    , cryptoContextExpiryMs :: Int
    }

type LlnwApiConfig
  = { streamAuthTypeUrl :: String
    , streamAuthUrl :: String
    , streamPublishUrl :: String
    , slotLookupUrl :: String
    }

type LoadMonitorConfig
  = {loadAnnounceMs :: Int}


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

nodeConfig :: Effect Node.Config
nodeConfig = do
  getMandatoryRecord "nodeConfig"

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

ingestInstanceConfig :: Effect IngestInstanceConfig
ingestInstanceConfig = do
  getMandatoryRecord "ingestInstanceConfig"

egestAgentConfig :: Effect EgestAgentConfig
egestAgentConfig = do
  getMandatoryRecord "egestConfig"

rtmpIngestConfig :: Effect RtmpIngestConfig
rtmpIngestConfig = do
  getMandatoryRecord "rtmpIngestConfig"

llnwApiConfig :: Effect LlnwApiConfig
llnwApiConfig = do
  getMandatoryRecord "llnwApiConfig"

loadMonitorConfig :: Effect LoadMonitorConfig
loadMonitorConfig = do
  getMandatoryRecord "loadMonitorConfig"


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
    Left error ->
      do
        _ <- Logger.error "Invalid Config" {misc: {key: v,
                                                   error: error}}
        unsafeCrashWith ("invalid_config " <> v)
    Right ok ->
      pure $ ok
