-- -*- psc-ide-codegen: ("erl") -*-
module Erl.Utils
       (
         isRegistered,
         systemTime,
         TimeUnit(..)
       )
       where

import Effect (Effect)
import Erl.Atom (Atom, atom)

foreign import isRegisteredImpl :: Atom -> Effect Boolean
foreign import systemTimeImpl :: Atom -> Effect Int

data TimeUnit = MicroSecond
              | MilliSecond
              | Second

isRegistered :: String -> Effect Boolean
isRegistered name = isRegisteredImpl (atom name)

systemTime :: TimeUnit -> Effect Int
systemTime MicroSecond = systemTimeImpl (atom "microsecond")
systemTime MilliSecond = systemTimeImpl (atom "millisecond")
systemTime Second = systemTimeImpl (atom "second")
