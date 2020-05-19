module Rtsv2.Utils
  ( member
  , noprocToMaybe
  , badargToMaybe
  , crashIfLeft
  , undefined
  , cryptoStrongBytes
  , cryptoStrongToken
  , chainEither
  , chainIntoEither
  ) where

import Prelude

import Data.Either (Either(..), either)
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
foreign import badargToMaybeImpl :: forall a. Maybe a -> (a -> Maybe a) -> Effect a -> Effect (Maybe a)

noprocToMaybe :: forall a. Effect a -> Effect (Maybe a)
noprocToMaybe = noprocToMaybeImpl Nothing Just

badargToMaybe :: forall a. Effect a -> Effect (Maybe a)
badargToMaybe = badargToMaybeImpl Nothing Just

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

chainEither :: forall l a b. (a -> Effect (Either l b)) -> Either l a -> Effect (Either l b)
chainEither = either (pure <<< Left)

chainIntoEither :: forall l a b. (a -> Effect b) -> Either l a -> Effect (Either l b)
chainIntoEither = chainEither <<< ( (map Right) <<< _ )
