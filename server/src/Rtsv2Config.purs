module Rtsv2Config
  ( webPort
  , configuredAgents
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
import Partial.Unsafe (unsafeCrashWith)
import Shared.Agents (Agent, strToAgent)

foreign import getEnv_ :: Atom -> Effect Foreign

webPort :: Effect Int
webPort = getMandatory readInt "web_port"

configuredAgents :: Effect (List Agent)
configuredAgents = do
  agentStrings <- getMandatory (readList >=> traverse readString) "agents"
  pure $ strToAgent <$> agentStrings

get :: forall a e. (Foreign -> ExceptT e Identity a) -> String -> Effect (Maybe a)
get f v = hush <<< runExcept <<< f <$> getEnv_ (atom v)

getMandatory :: forall a e. (Foreign -> ExceptT e Identity a) -> String -> Effect a
getMandatory f v = fromMaybe' (asyncCrashIfMissing v) <$> hush <<< runExcept <<< f <$> getEnv_ (atom v)

asyncCrashIfMissing :: forall a. String -> Unit -> a
asyncCrashIfMissing v = const $ unsafeCrashWith $ "Missing mandatory config value " <> v

getWithDefault :: forall a e. (Foreign -> ExceptT e Identity a) -> String -> a -> Effect a
getWithDefault f v d = fromMaybe d <$> (get f v)

-- TODO: Rehome with foreign or list
readList :: Foreign -> F (List Foreign)
readList = unsafeReadTagged "list"
