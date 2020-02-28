module Foreign.FrontEnd where

import Prelude

import Effect (Effect)

foreign import init_ :: Effect Unit

init :: Effect Unit
init = init_
