module Rtsv2Config where

import Erl.Data.List
import Prelude

import Agents (Agent(..))
import Control.Monad.Except (ExceptT, runExcept)
import Data.Either (hush)
import Data.Identity (Identity)
import Data.Maybe (Maybe, fromMaybe, maybe)
import Data.Newtype (wrap)
import Data.Traversable (traverse)
import Debug.Trace (spy, traceM)
import Effect (Effect)
import Erl.Atom (Atom, atom)
import Foreign (F, Foreign, readInt, readString, unsafeReadTagged)
import Partial (crashWith)
import Partial.Unsafe (unsafePartial)

foreign import getEnv_ :: Atom -> Effect Foreign

webPort :: Effect Int
webPort = getMandatory readInt "web_port"

initialAgents :: Effect (List Agent)
initialAgents = do
  agentStrings <- getMandatory (readList >=> traverse readString) "agents"
  pure $ unsafePartial $ strToAgent <$> agentStrings
  where
  strToAgent :: Partial => String -> Agent
  strToAgent "edgeAgent" = EdgeAgent

get :: forall a e. (Foreign -> ExceptT e Identity a) -> String -> Effect (Maybe a)
get f v = hush <<< runExcept <<< f <$> getEnv_ (atom v)


getMandatory :: forall a e. (Foreign -> ExceptT e Identity a) -> String -> Effect a
getMandatory f v =
  do
   let
    crashIfMissing :: Maybe a -> a
    crashIfMissing a = fromMaybe (unsafePartial $ crashWith $ "Missing mandatory config value " <> v) a
    foo = hush <<< runExcept <<< f <$> getEnv_ (atom v)
   _ <- traceM foo
   pure $ crashIfMissing <$> spy "wtf" foo


getWithDefault :: forall a e. (Foreign -> ExceptT e Identity a) -> String -> a -> Effect a
getWithDefault f v d = fromMaybe d <$> (get f v)

-- TODO: Rehome with foreign or list
readList :: Foreign -> F (List Foreign)
readList = unsafeReadTagged "list"
