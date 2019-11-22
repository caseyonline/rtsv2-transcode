module GlobalState
where


import Prelude

import Effect (Effect)
import Pinto (ServerName(..), StartLinkResult)
import Pinto.Gen (CallResult(..))
import Pinto.Gen as Gen


newtype State = State {}


type GlobalStateStartArgs = { }

serverName :: ServerName State
serverName = ServerName "globalState"

startLink :: GlobalStateStartArgs -> Effect StartLinkResult
startLink args =
  Gen.startLink serverName $ init args

isStreamAvailable :: String -> Effect Boolean
isStreamAvailable s = Gen.call serverName \state -> CallReply false state


init :: GlobalStateStartArgs -> Effect State
init args = do
  pure $ State {}
