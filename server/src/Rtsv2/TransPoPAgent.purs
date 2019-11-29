module Rtsv2.TransPoPAgent where

import Effect (Effect)
import Erl.Data.List (nil)
import Prelude
import Shared.Agent as Agents
import Pinto (ServerName(..), StartLinkResult)
import Pinto.Gen (CallResult(..))
import Pinto.Gen as Gen
import Gproc as Gproc

type State
  = {}

type Config
  = {
    }

serverName :: ServerName State Unit
serverName = Local "transPopAgent"

startLink :: Config -> Effect StartLinkResult
startLink args = Gen.startLink serverName (init args) Gen.defaultHandleInfo

init :: Config -> Effect State
init _ = do
  pure $ {}
