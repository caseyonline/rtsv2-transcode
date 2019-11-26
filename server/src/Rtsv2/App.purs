module Rtsv2.App where

import Effect.Uncurried (EffectFn2)
import Erl.Atom (Atom)
import Erl.Data.List (List)
import Erl.Data.Tuple (Tuple2)
import Pinto.App as App
import Rtsv2.Sup as Sup
import Erl.Process.Raw (Pid)

start :: forall a. EffectFn2 Atom (List a) (Tuple2 Atom Pid)
start = App.simpleStart Sup.startLink
