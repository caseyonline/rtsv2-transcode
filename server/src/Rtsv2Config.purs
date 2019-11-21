module Rtsv2Config where

import Prelude

import Agents (Agent(..))
import Erl.Data.List
import Data.Newtype (wrap)
import Effect (Effect)
import Partial.Unsafe (unsafePartial)

foreign import readString_ :: String -> Effect String
foreign import readStrings_ :: String -> Effect (List String)
foreign import readInt_ :: String -> Effect Int
foreign import readDirect_ :: forall a. String -> Effect a

webPort :: Effect Int
webPort =
  readInt_ "web_port"


initialAgents :: Effect (List Agent)
initialAgents = do
  agentStrings <- readStrings_ "agents"
  pure $ unsafePartial $ strToAgent <$> agentStrings
    where strToAgent :: Partial => String -> Agent
          strToAgent "edgeAgent" = EdgeAgent
