module PintoHelper
       ( exposeState
       , doExposeState
       ) where

import Prelude
import Effect (Effect)
import Pinto (ServerName)
import Pinto.Gen (CallResult(..))
import Pinto.Gen as Gen


exposeState :: forall a state msg. (state -> a) -> ServerName state msg -> Effect a
exposeState exposeFn serverName = Gen.doCall serverName
  \state -> pure $ CallReply (exposeFn state) state

doExposeState :: forall a state msg. (state -> Effect a) -> ServerName state msg -> Effect a
doExposeState exposeFn serverName = Gen.doCall serverName
  \state -> do
    result <- exposeFn state
    pure $ CallReply result state
