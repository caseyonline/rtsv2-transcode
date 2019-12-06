module EphemeralMap
       (
         EMap
       , empty
       , size
       , member
       , delete
       , keys
       , lookup
       , insert
       , insert'
       , values
       , garbageCollect
       , garbageCollect'
       )
       where

import Prelude

import Data.FoldableWithIndex (foldlWithIndex)
import Data.Maybe (Maybe)
import Effect (Effect)
import Erl.Data.List (List)
import Erl.Data.Map (Map)
import Erl.Data.Map as Map
import Erl.Data.Tuple (Tuple2, fst, snd, tuple2)
import Erl.Utils (Milliseconds)
import Erl.Utils as Erl

data EMap k v = EMap (Map k (Tuple2 v Milliseconds))

empty :: forall k v. EMap k v
empty = EMap Map.empty

size :: forall k v. EMap k v -> Int
size (EMap m) = Map.size m

member :: forall k v. k -> EMap k v -> Boolean
member k (EMap m) = Map.member k m

delete :: forall k v. k -> EMap k v -> EMap k v
delete k (EMap m) = EMap $ Map.delete k m

keys :: forall k v. EMap k v -> List k
keys (EMap m) = Map.keys m

lookup :: forall k v. k -> EMap k v -> Maybe v
lookup k (EMap m) = fst <$> Map.lookup k m

insert :: forall k v. k -> v -> Milliseconds -> EMap k v -> EMap k v
insert k v ms (EMap m) = EMap $ Map.insert k (tuple2 v ms) m

insert' :: forall k v. k -> v -> EMap k v -> Effect (EMap k v)
insert' k v m =
  do
    now <- Erl.systemTimeMs
    pure $ insert k v now m

values :: forall k v. EMap k v -> List v
values (EMap m) = fst <$> Map.values m

garbageCollect :: forall k v. Milliseconds -> EMap k v -> EMap k v
garbageCollect threshold (EMap m) =
  EMap $ foldlWithIndex (\k acc v ->
                          if (snd v) < threshold then acc
                          else Map.insert k v acc
                        )
                        Map.empty
                        m

garbageCollect' :: forall k v. Milliseconds -> EMap k v -> Effect (EMap k v)
garbageCollect' age m =
  do
    now <- Erl.systemTimeMs
    let
      threshold = now - age
    pure $ garbageCollect threshold m
