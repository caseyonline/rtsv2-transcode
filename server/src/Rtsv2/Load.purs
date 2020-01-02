module Rtsv2.Load
       ( percentageToLoad
       , startLink
       , load
       , setLoad
       ) where

import Prelude

import Effect (Effect)
import Pinto (ServerName(..), StartLinkResult)
import Pinto.Gen (CallResult(..))
import Pinto.Gen as Gen
import Rtsv2.IntraPoPAgent as IntraPopAgent
import Shared.Types (Load(..))

percentageToLoad :: Number -> Load
percentageToLoad percentage
  | percentage < 25.0 = Idle
  | percentage < 50.0 = Busy
  | percentage < 75.0 = Overloaded
  | otherwise = Critical

type State =
  {
    load :: Number
  }

serverName :: ServerName State Unit
serverName = Local "load"

startLink :: Unit -> Effect StartLinkResult
startLink args =
  Gen.startLink serverName (init args) Gen.defaultHandleInfo

init :: Unit -> Effect State
init args =
  pure $ {load: 0.0}

load :: Effect Load
load =
  Gen.call serverName \state@{ load: currentLoad } -> CallReply (percentageToLoad currentLoad) state

setLoad :: Number -> Effect Unit
setLoad newLoad = do
  _ <- IntraPopAgent.announceLoad (percentageToLoad newLoad)
  Gen.call serverName \state -> CallReply unit state{load = newLoad}
