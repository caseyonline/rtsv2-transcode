module Ephemeral.MultiMap
       ( EMultiMap
       , empty
       , size
       , member
       , delete
       , keys
       , lookup
       , insert
       , insert'
       --, values
       , garbageCollect
       , garbageCollect'
       )
       where

import Prelude

import Data.FoldableWithIndex (foldlWithIndex)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (wrap)
import Effect (Effect)
import Ephemeral (EData)
import Ephemeral as EData
import Ephemeral.List (EList)
import Ephemeral.List as EList
import Erl.Data.List (List, nil)
import Erl.Data.Map (Map)
import Erl.Data.Map as Map
import Erl.Utils (Milliseconds)
import Erl.Utils as Erl

--TODO - this should really be EMultiMap (Map k (ENonEmptyList v))
data EMultiMap k v = EMultiMap (Map k (EList v))

empty :: forall k v. EMultiMap k v
empty = EMultiMap Map.empty

size :: forall k v. EMultiMap k v -> Int
size (EMultiMap m) = Map.size m

member :: forall k v. k -> EMultiMap k v -> Boolean
member k (EMultiMap m) = Map.member k m

delete :: forall k v. Eq v => k -> v -> EMultiMap k v -> EMultiMap k v
delete k v (EMultiMap m) =
  let
    deleteMember :: Maybe (EList v) -> Maybe (EList v)
    deleteMember Nothing = Nothing
    deleteMember (Just list) = let list2 = EList.delete (EData.new (wrap 0) v) list
--    deleteMember (Just list) = let list2 = EList.delete' v list
                               in
                                if EList.null list2 then Nothing
                                else Just list2
  in
   EMultiMap $ Map.alter deleteMember k m

keys :: forall k v. EMultiMap k v -> List k
keys (EMultiMap m) = Map.keys m

lookup :: forall k v. k -> EMultiMap k v -> List v
lookup k (EMultiMap m) =
  fromMaybe nil $ EList.extract <$> Map.lookup k m

insert :: forall k v. Eq v => k -> EData v -> EMultiMap k v -> EMultiMap k v
insert k ev (EMultiMap m) =
  let addMember (Just evs) = Just $ EList.insert ev evs
      addMember Nothing = Just $ EList.singleton ev
  in
  EMultiMap $ Map.alter addMember k m

insert' :: forall k v. Eq v => k -> v -> EMultiMap k v -> Effect (EMultiMap k v)
insert' k v m =
  do
    ev <- EData.new' v
    pure $ insert k ev m

-- values :: forall k v. EMultiMap k v -> List (Set v)
-- values (EMultiMap m) = let foo = Map.values m
--                          bar = (<$>) EData.extract
--                        in EData.extract <$> ?foo

garbageCollect :: forall k v. Milliseconds -> EMultiMap k v -> EMultiMap k v
garbageCollect threshold (EMultiMap m) =
  EMultiMap $ foldlWithIndex
  (\k acc v -> let filtered = EList.garbageCollect threshold v
               in if EList.null filtered then acc
                  else Map.insert k filtered acc
  )
  Map.empty
  m

garbageCollect' :: forall k v. Milliseconds -> EMultiMap k v -> Effect (EMultiMap k v)
garbageCollect' age m =
  do
    now <- Erl.systemTimeMs
    let
      threshold = now - age
    pure $ garbageCollect threshold m
