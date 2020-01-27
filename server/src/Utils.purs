module Rtsv2.Utils
  ( member
  , noprocToMaybe
  , crashIfLeft
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Effect (Effect)
import Erl.Data.List (List, findIndex)
import Partial.Unsafe (unsafePartial)

member :: forall a. Eq a => a -> List a -> Boolean
member a as = fromMaybe false $ const true <$> findIndex ((==) a) as

foreign import noprocToMaybeImpl :: forall a. Maybe a -> (a -> Maybe a) -> Effect a -> Effect (Maybe a)

noprocToMaybe :: forall a. Effect a -> Effect (Maybe a)
noprocToMaybe = noprocToMaybeImpl Nothing Just


crashIfLeft :: forall e a m. Monad m => Either e a -> m a
crashIfLeft either = unsafePartial $
  case either of Right a -> pure a
