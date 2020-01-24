module Erl.Utils
       ( systemTimeMs
       , sleep
       , makeRef
       , Milliseconds
       , Url
       , Ref
       )
       where

import Prelude

import Data.Newtype (class Newtype, unwrap, wrap)
import Effect (Effect)
import Erl.Atom (Atom, atom)
import Foreign (Foreign)

foreign import systemTimeImpl :: Atom -> Effect Int
foreign import sleepImpl :: Int -> Effect Unit
foreign import makeRefImpl :: Foreign

sleep :: Milliseconds -> Effect Unit
sleep = sleepImpl <<< unwrap

-- TODO - find a place for these utility types to live (a la id3as_common?)
-- | A duration measured in milliseconds.
newtype Milliseconds = Milliseconds Int

derive instance newtypeMilliseconds :: Newtype Milliseconds _
derive newtype instance eqMilliseconds :: Eq Milliseconds
derive newtype instance ordMilliseconds :: Ord Milliseconds


-- | Url type
newtype Url = Url String
derive instance newtypeURL :: Newtype Url _
derive newtype instance eqURL :: Eq Url
derive newtype instance ordURL :: Ord Url


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

systemTimeMs :: Effect Milliseconds
systemTimeMs = wrap <$> systemTimeImpl (atom "millisecond")

newtype Ref = Ref Foreign

instance eqRef :: Eq Ref where
  eq _ _ = true

makeRef :: Ref
makeRef = Ref makeRefImpl
