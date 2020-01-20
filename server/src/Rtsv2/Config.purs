module Rtsv2.Config
  ( GlobalConfig
  , IngestAggregatorAgentConfig
  , WebConfig
  , PoPDefinitionConfig
  , IntraPoPAgentConfig
  , TransPoPAgentConfig
  , EgestAgentConfig
  , IntraPoPAgentApi
  , TransPoPAgentApi
  , LlnwApiConfig
  , LoadMonitorConfig
  , webConfig
  , globalConfig
  , nodeConfig
  , popDefinitionConfig
  , intraPoPAgentConfig
  , transPoPAgentConfig
  , ingestAggregatorAgentConfig
  , egestAgentConfig
  , rtmpIngestConfig
  , llnwApiConfig
  , loadMonitorConfig
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
import Erl.Data.List (List, singleton)
import Foreign (F, Foreign, readString, unsafeReadTagged)
import Logger (Logger)
import Logger as Logger
import Partial.Unsafe (unsafeCrashWith)
import Rtsv2.Node as Node
import Shared.Agent (Agent, strToAgent)
import Shared.Stream (StreamId)
import Shared.Types (LocatedServer(..), ServerAddress)
import Shared.Utils (lazyCrashIfMissing)
import Simple.JSON (class ReadForeign, readImpl)


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

type IngestAggregatorAgentConfig
  = { streamAvailableAnnounceMs :: Int
    , shutdownLingerTimeMs :: Int}

type EgestAgentConfig
  = { egestAvailableAnnounceMs :: Int
    , lingerTimeMs :: Int
    }

type IntraPoPAgentConfig
  = { bindPort :: Int
    , rpcPort :: Int
    , rejoinEveryMs :: Int
    , expireThresholdMs :: Int
    , expireEveryMs :: Int
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
    }

type IntraPoPAgentApi
  = { announceRemoteStreamIsAvailable :: StreamId -> LocatedServer -> Effect Unit
    , announceRemoteStreamStopped :: StreamId -> LocatedServer -> Effect Unit
    , announceTransPoPLeader :: Effect Unit
    }

type TransPoPAgentApi
  = { announceStreamIsAvailable :: StreamId -> LocatedServer -> Effect Unit
    , announceStreamStopped :: StreamId -> LocatedServer -> Effect Unit
    , handleRemoteLeaderAnnouncement :: LocatedServer -> Effect Unit
    }

type RtmpIngestConfig
  = { port :: Int
    , nbAcceptors :: Int
    }

type LlnwApiConfig
  = { streamAuthTypeUrl :: String
    , streamAuthUrl :: String
    , streamPublishUrl :: String
    }

type LoadMonitorConfig
  = {loadAnnounceMs :: Int}

foreign import getEnv_ :: Atom -> Effect Foreign
foreign import getMap_ :: Atom -> Effect Foreign
foreign import mergeOverrides :: Effect Foreign

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


--------------------------------------------------------------------------------
-- Log helpers
--------------------------------------------------------------------------------
domains :: List Atom
domains = atom "Config" # singleton

--logInfo :: forall a. Logger a
--logInfo = domainLog Logger.info

--logWarning :: forall a. Logger a
--logWarning = domainLog Logger.warning

logError :: forall a. Logger a
logError = domainLog Logger.warning

domainLog :: forall a. Logger {domain :: List Atom, misc :: a} -> Logger a
domainLog = Logger.doLog domains
