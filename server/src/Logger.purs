-- -*- psc-ide-codegen: ("erl") -*-
module Logger
       ( emergency
       , alert
       , critical
       , error
       , warning
       , notice
       , info
       , debug
       , spy
       ) where

import Prelude

import Debug.Trace (class DebugWarning)
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import Foreign (Foreign)

foreign import emergency :: forall a. String -> a -> Effect Foreign
foreign import alert :: forall a. String -> a -> Effect Foreign
foreign import critical :: forall a. String -> a -> Effect Foreign
foreign import error :: forall a. String -> a -> Effect Foreign
foreign import warning :: forall a. String -> a -> Effect Foreign
foreign import notice :: forall a. String -> a -> Effect Foreign
foreign import info :: forall a. String -> a -> Effect Foreign
foreign import debug :: forall a. String -> a -> Effect Foreign
foreign import spyImpl :: forall a. String -> a -> Effect Foreign

spy :: forall a. DebugWarning => String -> a -> a
spy str a = unsafePerformEffect do
  _ <-  spyImpl str {misc : a}
  pure a
