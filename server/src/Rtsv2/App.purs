module Rtsv2.App where

import Prelude

import Effect (Effect)
import Effect.Uncurried (EffectFn1, EffectFn2, mkEffectFn1)
import Effect.Unsafe (unsafePerformEffect)
import Erl.Atom (Atom)
import Erl.Data.List (List, nil)
import Foreign (Foreign, unsafeToForeign)
import Logger as Logger
import Pinto.App as App
import Rtsv2.Config (mergeOverrides)
import Rtsv2.Top as Top

foreign import setLogRootImpl :: Effect Foreign
foreign import enableSchedulerWallTimeImpl :: Effect Foreign


start :: forall a. EffectFn2 Atom (List a) Foreign
start =
  let
    _ = unsafePerformEffect enableSchedulerWallTimeImpl
    _ = unsafePerformEffect mergeOverrides
  in
   App.simpleStart Top.startLink

prep_stop :: forall a. EffectFn1 a Foreign
prep_stop = mkEffectFn1 (\_state -> do
                            void $ (Logger.info <<< Logger.traceMetadata nil) "RTS-V2 Application prep-stop" {}
                            Top.stop
                            void $ (Logger.info <<< Logger.traceMetadata nil) "RTS-V2 Application Top gen-server stopped" {}
                            pure $ unsafeToForeign 1
                        )
