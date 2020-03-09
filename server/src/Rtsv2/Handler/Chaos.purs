module Rtsv2.Handler.Chaos
       ( chaos
       ) where

import Prelude

import Effect (Effect)
import Shared.Types (ChaosPayload)
import StetsonHelper (PostHandler, processPostPayload)

foreign import chaosImpl :: ChaosPayload -> Effect Unit

chaos :: PostHandler ChaosPayload
chaos = processPostPayload chaosImpl
