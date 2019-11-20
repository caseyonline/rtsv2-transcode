module Gproc where

import Effect (Effect)

foreign import registered_ :: forall a. a -> Effect Boolean

registered :: forall a. a -> Effect Boolean
registered name = registered_ name
