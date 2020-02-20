module Rtsv2.Utils
  ( member
  , noprocToMaybe
  , crashIfLeft
  , undefined
  , cryptoStrongBytes
  , cryptoStrongToken
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Int (round, toNumber)
import Data.Maybe (Maybe(..), fromMaybe)
import Effect (Effect)
import Erl.Data.Binary (Binary)
import Erl.Data.List (List, findIndex)
import Partial.Unsafe (unsafePartial)
import Unsafe.Coerce as Unsafe.Coerce

member :: forall a. Eq a => a -> List a -> Boolean
member a as = fromMaybe false $ const true <$> findIndex ((==) a) as

foreign import noprocToMaybeImpl :: forall a. Maybe a -> (a -> Maybe a) -> Effect a -> Effect (Maybe a)

noprocToMaybe :: forall a. Effect a -> Effect (Maybe a)
noprocToMaybe = noprocToMaybeImpl Nothing Just

crashIfLeft :: forall e a m. Monad m => Either e a -> m a
crashIfLeft either = unsafePartial $
  case either of Right a -> pure a

undefined :: forall a. a
undefined = Unsafe.Coerce.unsafeCoerce unit

foreign import cryptoStrongBytes :: Int -> Effect Binary
foreign import binaryToBase64 :: Binary -> String
foreign import binaryToHexStr :: Binary -> String

cryptoStrongToken :: Int -> Effect String
cryptoStrongToken n =
  binaryToBase64 <$> cryptoStrongBytes (round ((toNumber n) / 1.333))
