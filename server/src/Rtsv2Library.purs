module Rtsv2Library where

import Prelude

import Effect (Effect)
import Pinto (ServerName(..), StartLinkResult)
import Pinto.Gen as Gen

type Rtsv2LibraryStartArgs = {
}

type State = {
}

serverName :: ServerName State
serverName = ServerName "pure_library"

startLink :: Rtsv2LibraryStartArgs -> Effect StartLinkResult
startLink args =
  Gen.startLink serverName $ init args

init :: Rtsv2LibraryStartArgs -> Effect State
init args = do
  pure $ {}
