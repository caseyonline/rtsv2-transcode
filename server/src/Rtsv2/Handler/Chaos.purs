module Rtsv2.Handler.Chaos
       ( chaos
       ) where

import Prelude

import Effect (Effect)
import StetsonHelper (PostHandler, processPostPayload)

foreign import chaosImpl :: ChaosPayload -> Effect Unit

type ChaosPayload =
  { name :: String
  , num_hits :: Int
  , delay_between_hits_ms :: Int
  }

chaos :: PostHandler ChaosPayload
chaos = processPostPayload chaosImpl
