module Erl.Utils
       ( systemTimeMs
       , sleep
       , makeRef
       , privDir
       , Milliseconds
       , Url
       , Ref
       )
       where

import Prelude

import Data.Newtype (class Newtype, unwrap, wrap)
import Effect (Effect)
import Erl.Atom (Atom, atom)

foreign import systemTimeImpl :: Atom -> Effect Int
foreign import sleepImpl :: Int -> Effect Unit
foreign import makeRefImpl :: Effect Ref
foreign import privDirImpl :: Atom -> String
foreign import eqRefImpl :: Ref -> Ref -> Boolean
foreign import data Ref :: Type

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



instance eqRef :: Eq Ref where
  eq = eqRefImpl

makeRef :: Effect Ref
makeRef = makeRefImpl

privDir :: Atom -> String
privDir = privDirImpl
