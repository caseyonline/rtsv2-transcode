module Rtsv2.Handler.EgestStats
       (
         stats
       ) where

import Prelude

import Data.Maybe (Maybe(..))
import Rtsv2.Agents.EgestInstance as EgestInstance
import Shared.Stream (EgestKey(..), SlotId)
import Shared.Types.Agent.State as PublicState
import StetsonHelper (GetResponse, jsonResponse)

stats :: SlotId -> GetResponse PublicState.Egest
stats = jsonResponse <<< (map Just) <<< EgestInstance.currentStats <<< EgestKey
