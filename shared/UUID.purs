module Shared.UUID
       (
         UUID
       , fromString
       , empty
       ) where

import Prelude

import Control.Monad.Except (except, mapExcept)
import Data.Either (Either(..), either)
import Data.List.NonEmpty as NEL
import Data.Maybe (Maybe, maybe)
import Data.Newtype (class Newtype)
import Foreign (Foreign, ForeignError(..), tagOf, unsafeToForeign)
import Simple.JSON (class ReadForeign, class WriteForeign, readImpl)

-- | UUID type
foreign import data UUID :: Type

foreign import eqUUIDImpl :: UUID -> UUID -> Boolean
foreign import compareUUIDImpl :: UUID -> UUID -> Ordering
foreign import stringToUUIDImpl :: String -> Maybe UUID
foreign import uuidToStringImpl :: UUID -> String
foreign import emptyImpl :: UUID

instance eqUUID :: Eq UUID where eq = eqUUIDImpl
instance ordUUID :: Ord UUID where compare = compareUUIDImpl
instance showUUID :: Show UUID where show = uuidToStringImpl
instance readForeignUUID :: ReadForeign UUID where
  readImpl value =
    let
      fromUuid = maybe error pure <<< stringToUUIDImpl
      error = Left $ NEL.singleton $ TypeMismatch "UUID" (tagOf value)
    in
     mapExcept (either (const error) fromUuid) (readImpl value)

instance writeForeignRef :: WriteForeign UUID where
  writeImpl = unsafeToForeign <<< uuidToStringImpl

fromString :: String -> Maybe UUID
fromString = stringToUUIDImpl

empty :: UUID
empty = emptyImpl
