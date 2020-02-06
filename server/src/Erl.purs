module Erl.Utils
       ( systemTimeMs
       , sleep
       , makeRef
       , privDir
       , Url
       , Ref
       )
       where

import Prelude

import Data.Newtype (class Newtype, unwrap, wrap)
import Effect (Effect)
import Erl.Atom (Atom, atom)
import Shared.Types (Milliseconds)

foreign import systemTimeImpl :: Atom -> Effect Int
foreign import sleepImpl :: Int -> Effect Unit
foreign import makeRefImpl :: Effect Ref
foreign import privDirImpl :: Atom -> String
foreign import eqRefImpl :: Ref -> Ref -> Boolean
foreign import data Ref :: Type

sleep :: Milliseconds -> Effect Unit
sleep = sleepImpl <<< unwrap

-- TODO - find a place for these utility types to live (a la id3as_common?)

-- | Url type
newtype Url = Url String
derive instance newtypeURL :: Newtype Url _
derive newtype instance eqURL :: Eq Url
derive newtype instance ordURL :: Ord Url

systemTimeMs :: Effect Milliseconds
systemTimeMs = wrap <$> systemTimeImpl (atom "millisecond")

instance eqRef :: Eq Ref where
  eq = eqRefImpl

makeRef :: Effect Ref
makeRef = makeRefImpl

privDir :: Atom -> String
privDir = privDirImpl
