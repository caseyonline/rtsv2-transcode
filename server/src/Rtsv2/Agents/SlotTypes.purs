module Rtsv2.Agents.SlotTypes
       ( SlotConfiguration
       , SlotProfile
       )
       where

import Erl.Data.List (List)

type SlotConfiguration =
  { profiles :: List SlotProfile
  }

type SlotProfile =
  { name :: String
  , firstAudioSSRC :: Int
  , firstVideoSSRC :: Int
  }
