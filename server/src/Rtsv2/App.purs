module Rtsv2.App where

import Effect (Effect)
import Effect.Uncurried (EffectFn2)
import Effect.Unsafe (unsafePerformEffect)
import Erl.Atom (Atom)
import Erl.Data.List (List)
import Erl.Data.Tuple (Tuple2)
import Foreign (Foreign)
import Pinto.App as App
import Rtsv2.Config (mergeOverrides)
import Rtsv2.Sup as Sup

foreign import setLogRoot :: Effect Foreign


start :: forall a. EffectFn2 Atom (List a) (Tuple2 Atom Foreign)
start =
  let
--    _ = unsafePerformEffect setLogRoot
    _ = unsafePerformEffect mergeOverrides
  in
   App.simpleStart Sup.startLink
