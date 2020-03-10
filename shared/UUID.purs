module Shared.UUID
       (
         UUID
       , fromString
       , empty
       ) where

import Prelude

import Control.Monad.Except (mapExcept)
import Data.Either (Either(..), either)
import Data.List.NonEmpty as NEL
import Data.Maybe (Maybe(..), maybe)
import Foreign (ForeignError(..), tagOf, unsafeToForeign)
import Simple.JSON (class ReadForeign, class WriteForeign, readImpl)

-- | UUID type
foreign import data UUID :: Type

foreign import eqUUIDImpl :: UUID -> UUID -> Boolean
foreign import compareUUIDImpl :: Ordering -> Ordering -> Ordering -> UUID -> UUID -> Ordering
foreign import stringToUUIDImpl :: (Maybe UUID) -> (UUID -> Maybe UUID) -> String -> Maybe UUID
foreign import uuidToStringImpl :: UUID -> String
foreign import emptyImpl :: UUID

instance eqUUID :: Eq UUID where eq = eqUUIDImpl
instance ordUUID :: Ord UUID where compare = compareUUIDImpl LT EQ GT
instance showUUID :: Show UUID where show = uuidToStringImpl
instance readForeignUUID :: ReadForeign UUID where
  readImpl value =
    let
      fromUuid = maybe error pure <<< (stringToUUIDImpl Nothing Just)
      error = Left $ NEL.singleton $ TypeMismatch "UUID" (tagOf value)
    in
     mapExcept (either (const error) fromUuid) (readImpl value)

instance writeForeignRef :: WriteForeign UUID where
  writeImpl = unsafeToForeign <<< uuidToStringImpl

fromString :: String -> Maybe UUID
fromString = stringToUUIDImpl Nothing Just

empty :: UUID
empty = emptyImpl
