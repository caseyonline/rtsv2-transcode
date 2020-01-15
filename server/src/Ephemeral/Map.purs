module Ephemeral.Map
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
       , garbageCollect2
       , garbageCollect'
       )
       where

import Prelude

import Data.Bifunctor (lmap)
import Data.FoldableWithIndex (foldlWithIndex)
import Data.Maybe (Maybe)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Ephemeral (EData, expired, extract)
import Ephemeral as EData
import Erl.Data.List (List, nil, (:))
import Erl.Data.Map (Map)
import Erl.Data.Map as Map
import Erl.Utils (Milliseconds)
import Erl.Utils as Erl


--multimap
--data EMM f v = EMM (Map k (f Edata v)


data EMap k v = EMap (Map k (EData v))

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
lookup k (EMap m) = EData.extract <$> Map.lookup k m

insert :: forall k v. k -> EData v -> EMap k v -> EMap k v
insert k ev (EMap m) = EMap $ Map.insert k ev m

insert' :: forall k v. k -> v -> EMap k v -> Effect (EMap k v)
insert' k v m =
  do
    now <- Erl.systemTimeMs
    pure $ insert k (EData.new now v) m

values :: forall k v. EMap k v -> List v
values (EMap m) = EData.extract <$> Map.values m

garbageCollect :: forall k v. Milliseconds -> EMap k v -> EMap k v
garbageCollect threshold (EMap m) =
  EMap $ foldlWithIndex
  (\k acc v -> if expired threshold v then acc
               else  Map.insert k v acc
  )
  Map.empty
  m

garbageCollect2 :: forall k v. Milliseconds -> EMap k v -> Tuple (EMap k v) (List (Tuple k v))
garbageCollect2 threshold (EMap m) =
  lmap EMap $ foldlWithIndex
  (\k (Tuple acc garbage) v ->
    if expired threshold v then (Tuple acc ((Tuple k (extract v)) : garbage))
    else Tuple (Map.insert k v acc) garbage
  )
  (Tuple Map.empty nil)
  m

garbageCollect' :: forall k v. Milliseconds -> EMap k v -> Effect (EMap k v)
garbageCollect' age m =
  do
    now <- Erl.systemTimeMs
    let
      threshold = now - age
    pure $ garbageCollect threshold m
