module PintoHelper
       ( exposeState
       ) where

import Prelude
import Effect (Effect)
import Pinto (ServerName)
import Pinto.Gen (CallResult(..))
import Pinto.Gen as Gen


exposeState :: forall a state msg. (state -> a) -> ServerName state msg -> Effect a
exposeState exposeFn serverName = Gen.doCall serverName
  \state -> pure $ CallReply (exposeFn state) state
