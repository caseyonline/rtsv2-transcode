module Shared.Utils
  ( lazyCrashIfMissing
  , distinctRandomNumbers
  ) where

import Prelude

import Control.Apply (lift2)
import Data.Set as Set
import Effect (Effect)
import Effect.Random (randomInt)
import Erl.Data.List (List, fromFoldable)
import Partial.Unsafe (unsafeCrashWith)

lazyCrashIfMissing :: forall a. String -> Unit -> a
lazyCrashIfMissing s = \unit -> unsafeCrashWith s

distinctRandomNumbers :: Int -> Int -> Effect (List Int)
distinctRandomNumbers numRequired maxValue =
  randomNumbers_ numRequired maxValue (pure Set.empty)
  <#> fromFoldable

randomNumbers_ :: Int -> Int -> Effect (Set.Set Int) -> Effect (Set.Set Int)
randomNumbers_ numRequired maxValue set =
  let
    addDistinctRand :: Effect (Set.Set Int)
    addDistinctRand =
      lift2 Set.insert (randomInt 0 maxValue) set
  in
   do
     set2 <- addDistinctRand
     if Set.size set2 == numRequired then
       pure $ set2
     else
       randomNumbers_ numRequired maxValue (pure set2)
