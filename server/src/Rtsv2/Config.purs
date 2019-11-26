module Rtsv2.Config
  ( webConfig
  , configuredAgents
  , popDefinitionConfig
  , intraPoPAgentConfig
  ) where

import Prelude

import Control.Monad.Except (ExceptT, runExcept)
import Data.Either (hush)
import Data.Identity (Identity)
import Data.Maybe (Maybe, fromMaybe, fromMaybe')
import Data.Traversable (traverse)
import Effect (Effect)
import Erl.Atom (Atom, atom)
import Erl.Data.List (List)
import Foreign (F, Foreign, readInt, readString, unsafeReadTagged)
import Shared.Agents (Agent, strToAgent)
import Shared.Utils (lazyCrashIfMissing)
import Rtsv2.IntraPoPAgent as IntraPoP
import Rtsv2.Web as Web
import Rtsv2.PoPDefinition as PoPDefinition

foreign import getEnv_ :: Atom -> Effect Foreign

--webPort :: Effect Int
--webPort = getMandatory readInt "web_port"

webConfig :: Effect Web.StartArgs
webConfig = do
  config <- getMandatory (unsafeReadTagged "map") "httpApiConfig"
  pure $ config

configuredAgents :: Effect (List Agent)
configuredAgents = do
  agentStrings <- getMandatory (readList >=> traverse readString) "agents"
  pure $ strToAgent <$> agentStrings

popDefinitionConfig :: Effect PoPDefinition.StartArgs
popDefinitionConfig = do
  config <- getMandatory (unsafeReadTagged "map") "popDefinitionConfig"
  pure $ config

intraPoPAgentConfig :: Effect IntraPoP.StartArgs
intraPoPAgentConfig = do
  config <- getMandatory (unsafeReadTagged "map") "intraPoPConfig"
--  readprop rpcPort config
  pure $ config

get :: forall a e. (Foreign -> ExceptT e Identity a) -> String -> Effect (Maybe a)
get f v = hush <<< runExcept <<< f <$> getEnv_ (atom v)

getMandatory :: forall a e. (Foreign -> ExceptT e Identity a) -> String -> Effect a
getMandatory f v = fromMaybe' (lazyCrashIfMissing $ "Missing mandatory config value " <> v) <$> hush <<< runExcept <<< f <$> getEnv_ (atom v)

getWithDefault :: forall a e. (Foreign -> ExceptT e Identity a) -> String -> a -> Effect a
getWithDefault f v d = fromMaybe d <$> (get f v)

-- TODO: Rehome with foreign or list
readList :: Foreign -> F (List Foreign)
readList = unsafeReadTagged "list"
