-- -*- psc-ide-codegen: ("erl") -*-
module Rtsv2.Config
  ( webConfig
  , nodeConfig
  , popDefinitionConfig
  , intraPoPAgentConfig
  , transPoPAgentConfig
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
import Rtsv2.IntraPoPAgent as IntraPoP
import Rtsv2.TransPoPAgent as TransPoP
import Rtsv2.Node as Node
import Rtsv2.PoPDefinition as PoPDefinition
import Rtsv2.Web as Web
import Shared.Agent (Agent, strToAgent)
import Shared.Utils (lazyCrashIfMissing)
import Simple.JSON (class ReadForeign, readImpl)

foreign import getEnv_ :: Atom -> Effect Foreign
foreign import getMap_ :: Atom -> Effect Foreign

--webPort :: Effect Int
--webPort = getMandatory readInt "web_port"

webConfig :: Effect Web.Config
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

popDefinitionConfig :: Effect PoPDefinition.Config
popDefinitionConfig = do
  config <- getMandatory (unsafeReadTagged "map") "popDefinitionConfig"
  pure $ config

intraPoPAgentConfig :: Effect IntraPoP.Config
intraPoPAgentConfig = do
  getMandatoryRecord "intraPoPConfig"

transPoPAgentConfig :: Effect TransPoP.Config
transPoPAgentConfig = do
  getMandatoryRecord "transPoPConfig"

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