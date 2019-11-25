module Rtsv2IntraPoPAgent where

import Effect (Effect)
import Erl.Data.List (nil)
import Prelude
import Shared.Agents as Agents
import Pinto (ServerName(..), StartLinkResult)
import Pinto.Gen (CallResult(..))
import Pinto.Gen as Gen
import Logger as Logger
import Gproc as Gproc

type State
  = {}

type IntraPoPAgentStartArgs
  = {
    }

serverName :: ServerName State
serverName = ServerName "intraPopAgent"

startLink :: IntraPoPAgentStartArgs -> Effect StartLinkResult
startLink args = Gen.startLink serverName $ init args

init :: IntraPoPAgentStartArgs -> Effect State
init state = do
  _ <- Logger.info "Intra-PoP Agent Starting" {foo: "bar"}
  pure $ {}
