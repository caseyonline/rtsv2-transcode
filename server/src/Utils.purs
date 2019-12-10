module Rtsv2.Utils
  ( member
  ) where

import Prelude

import Data.Maybe (fromMaybe)
import Erl.Data.List (List, findIndex)

member :: forall a. Eq a => a -> List a -> Boolean
member a as = fromMaybe false $ const true <$> findIndex ((==) a) as
