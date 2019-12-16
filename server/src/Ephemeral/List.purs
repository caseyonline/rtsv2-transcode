module Ephemeral.List
       ( EList
       , empty
       , null
       , singleton
       , insert
       , delete
       , delete'
       , extract
       , garbageCollect
       , garbageCollect'
       ) where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Ephemeral (EData)
import Ephemeral as EData
import Erl.Data.List (List, nil, uncons, (:))
import Erl.Data.List as List
import Erl.Utils (Milliseconds)
import Erl.Utils as Erl

newtype EList a = EList (List (EData a))

empty :: forall a. EList a
empty = EList nil

null :: forall a. EList a -> Boolean
null (EList es) = List.null es

insert :: forall a. Eq a => EData a -> EList a -> EList a
insert ev (EList evs) = EList $ ev : List.delete ev evs

delete :: forall a. Eq a => EData a -> EList a -> EList a
delete ev (EList evs) = EList $ List.delete ev evs

delete' :: forall a. Eq a => a -> EList a -> EList a
delete' v (EList evs) =
  let
    delete_ evs' =
      case uncons evs' of
        Nothing -> nil
        Just {head, tail} ->
          if EData.extract head == v then evs'
          else head : delete_ tail
  in EList $ delete_ evs

extract :: forall a. EList a -> List a
extract (EList evs) = EData.extract <$> evs

singleton :: forall a. EData a -> EList a
singleton = EList <<< List.singleton

garbageCollect :: forall v. Milliseconds -> EList v -> EList v
garbageCollect threshold (EList evs) =
  EList $ List.filter (not (EData.expired threshold)) evs

garbageCollect' :: forall v. Milliseconds -> EList v -> Effect (EList v)
garbageCollect' age el =
  do
    now <- Erl.systemTimeMs
    let
      threshold = now - age
    pure $ garbageCollect threshold el
