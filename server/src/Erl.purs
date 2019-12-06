-- -*- psc-ide-codegen: ("erl") -*-
module Erl.Utils
       ( isRegistered
       , systemTimeMs
       , sleep
       , Milliseconds
       )
       where

import Prelude

import Data.Newtype (class Newtype, unwrap, wrap)
import Effect (Effect)
import Erl.Atom (Atom, atom)

foreign import isRegisteredImpl :: Atom -> Effect Boolean
foreign import systemTimeImpl :: Atom -> Effect Int
foreign import sleepImpl :: Int -> Effect Unit

sleep :: Milliseconds -> Effect Unit
sleep = sleepImpl <<< unwrap

-- | A duration measured in milliseconds.
newtype Milliseconds = Milliseconds Int

derive instance newtypeMilliseconds :: Newtype Milliseconds _
derive newtype instance eqMilliseconds :: Eq Milliseconds
derive newtype instance ordMilliseconds :: Ord Milliseconds

instance semigroupMilliseconds :: Semigroup Milliseconds where
  append (Milliseconds x) (Milliseconds y) = Milliseconds (x + y)

instance semiringMilliseconds :: Semiring Milliseconds where
  add (Milliseconds x) (Milliseconds y) = Milliseconds (x + y)
  zero = Milliseconds 0
  mul (Milliseconds x) (Milliseconds y) = Milliseconds (x * y)
  one = Milliseconds 1

instance ringMilliseconds :: Ring Milliseconds where
  sub (Milliseconds x) (Milliseconds y) = Milliseconds (x - y)

instance monoidMilliseconds :: Monoid Milliseconds where
  mempty = Milliseconds 0

instance showMilliseconds :: Show Milliseconds where
  show (Milliseconds n) = "(Milliseconds " <> show n <> ")"

isRegistered :: String -> Effect Boolean
isRegistered name = isRegisteredImpl (atom name)

systemTimeMs :: Effect Milliseconds
systemTimeMs = wrap <$> systemTimeImpl (atom "millisecond")
