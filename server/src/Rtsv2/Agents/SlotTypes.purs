module Rtsv2.Agents.SlotTypes
       ( SlotConfiguration
       , SlotProfile
       )
       where

import Prelude

import Erl.Data.List (List, nil, uncons, (:))

type SlotConfiguration =
  { profiles :: List SlotProfile
  }

type SlotProfile =
  { name :: String
  , firstAudioSSRC :: Int
  , firstVideoSSRC :: Int
  }
