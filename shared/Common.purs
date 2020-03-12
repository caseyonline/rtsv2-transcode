module Shared.Common
       (
         Milliseconds(..)
       , Url
       ) where

import Prelude

import Data.Newtype (class Newtype)
import Simple.JSON (class ReadForeign, class WriteForeign)

-- TODO - find a place for these utility types to live (a la id3as_common?)
-- | A duration measured in milliseconds.
newtype Milliseconds = Milliseconds Int

-- | Url type
newtype Url = Url String
derive newtype instance showUrl :: Show Url

------------------------------------------------------------------------------
-- Type class derivations
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- Milliseconds
derive instance newtypeMilliseconds :: Newtype Milliseconds _
derive newtype instance eqMilliseconds :: Eq Milliseconds
derive newtype instance ordMilliseconds :: Ord Milliseconds
derive newtype instance readForeignMilliseconds :: ReadForeign Milliseconds
derive newtype instance writeForeignMilliseconds :: WriteForeign Milliseconds
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

------------------------------------------------------------------------------
-- Url
derive instance newtypeURL :: Newtype Url _
derive newtype instance eqURL :: Eq Url
derive newtype instance ordURL :: Ord Url
derive newtype instance readForeignUrl :: ReadForeign Url
derive newtype instance writeForeignUrl :: WriteForeign Url
