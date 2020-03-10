module Rtsv2.Handler.Chaos
       ( chaos
       ) where

import Prelude

import Data.Newtype (unwrap)
import Effect (Effect)
import Erl.Atom (atom)
import Erl.Data.Tuple (tuple2, tuple3, tuple4)
import Foreign (Foreign, unsafeToForeign)
import Logger (spy)
import Shared.Chaos (ChaosGprocName(..), ChaosName(..), ChaosPayload)
import StetsonHelper (PostHandler, processPostPayload)

foreign import chaosImpl :: ChaosPayload -> Foreign -> Effect Unit

chaos :: PostHandler ChaosPayload
chaos = processPostPayload (\p@{name} -> chaosImpl p (spy "NAME" (chaosNameToErlang name)))

chaosNameToErlang :: ChaosName -> Foreign
chaosNameToErlang (Local string) = unsafeToForeign string
chaosNameToErlang (Gproc gprocName) = unsafeToForeign $ tuple3 (atom "n") (atom "l") (chaosGprocNameToErlang gprocName)

chaosGprocNameToErlang :: ChaosGprocName -> Foreign
chaosGprocNameToErlang (String str) = unsafeToForeign str
chaosGprocNameToErlang (Atom str) = unsafeToForeign (atom str)
chaosGprocNameToErlang (SlotId slotId) = unsafeToForeign (unwrap slotId)
chaosGprocNameToErlang (SlotRole slotRole) = unsafeToForeign slotRole
chaosGprocNameToErlang (GprocTuple2 fst snd) = unsafeToForeign (tuple2 (chaosGprocNameToErlang fst) (chaosGprocNameToErlang snd))
chaosGprocNameToErlang (GprocTuple3 fst snd thd) = unsafeToForeign (tuple3 (chaosGprocNameToErlang fst) (chaosGprocNameToErlang snd) (chaosGprocNameToErlang thd))
chaosGprocNameToErlang (GprocTuple4 fst snd thd fth) = unsafeToForeign (tuple4 (chaosGprocNameToErlang fst) (chaosGprocNameToErlang snd) (chaosGprocNameToErlang thd) (chaosGprocNameToErlang fth))
