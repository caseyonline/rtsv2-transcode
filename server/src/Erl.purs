-- -*- psc-ide-codegen: ("erl") -*-
module Erl.Utils
       (
         isRegistered
       )
       where

import Effect (Effect)
import Erl.Atom (Atom, atom)

foreign import isRegisteredImpl :: Atom -> Effect Boolean

isRegistered :: String -> Effect Boolean
isRegistered name = isRegisteredImpl (atom name)
