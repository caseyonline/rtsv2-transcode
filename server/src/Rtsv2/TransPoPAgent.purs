module Rtsv2.TransPoPAgent where

import Effect (Effect)
import Prelude
import Pinto (ServerName(..), StartLinkResult)
import Pinto.Gen as Gen

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
