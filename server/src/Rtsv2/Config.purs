
module Rtsv2.Config
  ( IngestAggregatorAgentConfig
  , WebConfig
  , PoPDefinitionConfig
  , IntraPoPAgentConfig
  , TransPoPAgentConfig
  , EdgeAgentConfig
  , webConfig
  , nodeConfig
  , popDefinitionConfig
  , intraPoPAgentConfig
  , transPoPAgentConfig
  , ingestAggregatorAgentConfig
  , edgeAgentConfig
  ) where

import Prelude

import Control.Monad.Except (ExceptT, runExcept)
import Data.Either (Either(..), hush)
import Data.Identity (Identity)
import Data.Maybe (Maybe, fromMaybe, fromMaybe')
import Data.Traversable (traverse)
--import Debug.Trace (spy)
import Effect (Effect)
import Erl.Atom (Atom, atom)
import Erl.Data.List (List)
import Foreign (F, Foreign, readString, unsafeReadTagged)
import Logger as Logger
import Partial.Unsafe (unsafeCrashWith)
import Rtsv2.Node as Node
import Shared.Agent (Agent, strToAgent)
import Shared.Utils (lazyCrashIfMissing)
import Simple.JSON (class ReadForeign, readImpl)

-- TODO - config should include BindIFace or BindIp
type WebConfig = { port :: Int }

type PoPDefinitionConfig
  = { popDefinitionFile :: String
    }

type IngestAggregatorAgentConfig
  = { streamAvailableAnnounceMs :: Int }

type EdgeAgentConfig
  = { edgeAvailableAnnounceMs :: Int
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
    , rejoinEveryMs :: Int
    , connectStreamAfterMs :: Int
    }

foreign import getEnv_ :: Atom -> Effect Foreign
foreign import getMap_ :: Atom -> Effect Foreign

webConfig :: Effect WebConfig
webConfig = do
  config <- getMandatory (unsafeReadTagged "map") "httpApiConfig"
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

edgeAgentConfig :: Effect EdgeAgentConfig
edgeAgentConfig = do
  getMandatoryRecord "edgeConfig"

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
