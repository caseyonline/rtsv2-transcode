module Shared.Common
       (
         Milliseconds(..)
       , Url(..)
       ) where

import Prelude

import Control.Monad.Except (except)
import Data.Either (note)
import Data.List.NonEmpty as NEL
import Data.Long (Long, toString)
import Data.Long as Long
import Data.Newtype (class Newtype, un)
import Foreign (ForeignError(..), readString, unsafeToForeign)
import Simple.JSON (class ReadForeign, class WriteForeign)

-- TODO - find a place for these utility types to live (a la id3as_common?)
-- | A duration measured in milliseconds.
newtype Milliseconds = Milliseconds Long

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
instance readForeignMilliseconds :: ReadForeign Milliseconds where
  readImpl = readString >=> parseMillis
    where
      error s = NEL.singleton (ForeignError (errorString s))
      errorString s = "Couldn't parse long: " <> s
      parseMillis s = except $ note (error s) $ Milliseconds <$> Long.fromString s

instance writeForeignMilliseconds :: WriteForeign Milliseconds where
  writeImpl = unsafeToForeign <<< toString <<< un Milliseconds

instance semigroupMilliseconds :: Semigroup Milliseconds where
  append (Milliseconds x) (Milliseconds y) = Milliseconds (x + y)

derive newtype instance semiringMilliseconds :: Semiring Milliseconds

instance ringMilliseconds :: Ring Milliseconds where
  sub (Milliseconds x) (Milliseconds y) = Milliseconds (x - y)

instance monoidMilliseconds :: Monoid Milliseconds where
  mempty = Milliseconds $ Long.fromInt 0

instance showMilliseconds :: Show Milliseconds where
  show (Milliseconds n) = "(Milliseconds " <> show n <> ")"

------------------------------------------------------------------------------
-- Url
derive instance newtypeURL :: Newtype Url _
derive newtype instance eqURL :: Eq Url
derive newtype instance ordURL :: Ord Url
derive newtype instance readForeignUrl :: ReadForeign Url
derive newtype instance writeForeignUrl :: WriteForeign Url
