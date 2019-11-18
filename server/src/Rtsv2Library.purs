module Rtsv2Library where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Effect (Effect)
import Erl.Data.List (List)
import Pinto (ServerName(..), StartLinkResult)
import Pinto.Gen (CallResult(..))
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
