module Network
  ( Neighbour(..)
  , Neighbours(..)
  , Network(..)
  , Edge(..)
  , Path(..)
  , pathsBetween
  , bestPaths
  , emptyNetwork
  , addEdge'
  , foldPath
  ) where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set as Set
import Data.Traversable (foldl)
import Data.Tuple (Tuple(..))
import Erl.Data.List (List, length, nil, reverse, uncons, (:))
import Erl.Data.Map (Map)
import Erl.Data.Map as Map

data Neighbour to = Neighbour to Int -- Cost of getting to neighbour is int

instance showNeighbour :: (Show a) => Show (Neighbour a) where
  show (Neighbour a c) = show a <> "(" <> show c <> ")"

newtype Neighbours a
  = Neighbours (Map a Int)

instance showNeighbours :: (Show a) => Show (Neighbours a) where
  show (Neighbours ns) = "Neighbours " <> show ns

newtype Network a
  = Network (Map a (Neighbours a))

instance showNetwork :: (Show a) => Show (Network a) where
  show (Network nt) = "Network " <> show nt

type Path a
  = { from :: a
    , to :: a
    , cost :: Int
    , length :: Int
    , via :: List a
    }

data Edge a c = Edge a a c

instance showEdge :: (Show a, Show c) => Show (Edge a c) where
  show (Edge a b c) = "Edge " <> show a <> "->" <> show b <> "(" <> show c <> ")"

emptyNetwork :: forall a. Ord a => Network a
emptyNetwork = Network Map.empty

emptyNeighbours :: forall a. Ord a => Neighbours a
emptyNeighbours = Neighbours $ Map.empty

addNeighbour :: forall a. Ord a => Neighbour a -> Neighbours a -> Neighbours a
addNeighbour (Neighbour to cost) (Neighbours ns) = Neighbours $ Map.insert to cost ns

addEdge' :: forall a. Ord a => a -> a -> Int -> Network a -> Network a
addEdge' from to c (Network nt) =
  let
    currentNeighbours = fromMaybe emptyNeighbours (Map.lookup from nt)
    newNeighbours = addNeighbour (Neighbour to c) currentNeighbours
  in
    Network $ Map.insert from newNeighbours nt

costPair :: forall a. Ord a => Path a -> Path a -> Int
costPair { cost: c1, via: v1 } { cost: c2, via: v2 } =
  -- Penalise any joint nodes in the via path hard and prefer short paths over long ones
  let
    numStepsInCommon = Set.size $ Set.intersection (Set.fromFoldable v1) (Set.fromFoldable v2)
  in
    (numStepsInCommon * 1000) + c1 + c2 + (10 * (length v1 + length v2))

bestPaths :: forall a. Ord a => List (Path a) -> Maybe { cost :: Int
                                                       , path1 :: Path a
                                                       , path2 :: Path a
                                                       }
bestPaths ps = nChoose2Fold
  (\a acc a' ->
    let
      costThisPair = costPair a a'
    in
      case acc of
        Nothing -> Just { cost: costThisPair, path1: a, path2:  a' }
        Just { cost: bestCost } ->
          if costThisPair < bestCost then
            Just { cost: costThisPair, path1: a, path2:  a' }
          else
            acc
  )
  Nothing
  ps

pathsBetween :: forall a. Show a => Ord a => Network a -> a -> a -> List (Path a)
pathsBetween (Network nt) from to =
  let
    initialCandidate :: Path a
    initialCandidate = { from: from, to: to, cost: 0, length: 0, via: nil }
  in
    routesBetween_ from initialCandidate nil (Set.singleton from)
  where
  routesBetween_ current candidate solutions alreadyVisited =
    if current == to then
      candidate { via = reverse candidate.via } : solutions
    else
      if candidate.length > 3 then
        solutions
      else
       case Map.lookup current nt of
        Nothing -> solutions
        Just (Neighbours ns) ->
          foldl
            ( \acc (Tuple a c) ->
                if Set.member a alreadyVisited then
                  acc
                else
                  let
                    nextCandadidate =
                      candidate
                        { via =
                          if a == to then
                            candidate.via
                          else
                            a : candidate.via
                        , cost = c + candidate.cost
                        , length = 1 + candidate.length
                        }
                  in
                    routesBetween_ a nextCandadidate acc (Set.insert a alreadyVisited)
            )
            solutions
            $ (Map.toUnfoldable ns :: List (Tuple a Int))

foldPath :: forall a b. (b -> a -> a -> b) -> b -> Path a -> b
foldPath f b { from: from, to: to, via: via } =
  let
    Tuple lastVia toEndofViaAcc =
      foldl (\(Tuple from' acc) to' -> Tuple to' (f acc from' to'))
        (Tuple from b)
        via
  in
    f toEndofViaAcc lastVia to


nChoose2Fold :: forall a acc. (a -> acc -> a -> acc) -> acc -> List a -> acc
nChoose2Fold f acc as =
  case uncons as of
    Nothing -> acc
    Just {head: a, tail} ->
      let acc' = foldl (f a) acc tail
      in nChoose2Fold f acc' tail
