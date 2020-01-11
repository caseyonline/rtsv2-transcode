module Rtsv2.Utils
  ( member
  , noprocToMaybe
  ) where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe)
import Effect (Effect)
import Erl.Data.List (List, findIndex)

member :: forall a. Eq a => a -> List a -> Boolean
member a as = fromMaybe false $ const true <$> findIndex ((==) a) as


foreign import noprocToMaybeImpl :: forall a. Maybe a -> (a -> Maybe a) -> Effect a -> Effect (Maybe a)

noprocToMaybe :: forall a. Effect a -> Effect (Maybe a)
noprocToMaybe = noprocToMaybeImpl Nothing Just
