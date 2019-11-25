module Rtsv2TransPoPAgent where

import Effect (Effect)
import Erl.Data.List (nil)
import Prelude
import Shared.Agents as Agents
import Pinto (ServerName(..), StartLinkResult)
import Pinto.Gen (CallResult(..))
import Pinto.Gen as Gen
import Gproc as Gproc

type State
  = {}

type TransPoPAgentStartArgs
  = {
    }

serverName :: ServerName State
serverName = ServerName "transPopAgent"

startLink :: TransPoPAgentStartArgs -> Effect StartLinkResult
startLink args = Gen.startLink serverName $ init args

init :: TransPoPAgentStartArgs -> Effect State
init _ = do
  pure $ {}
