module Rtsv2.Handler.EgestStats
       (
         stats
       ) where

import Prelude

import Data.Maybe (Maybe(..))
import Rtsv2.Agents.EgestInstance as EgestInstance
import Shared.Stream (EgestKey(..), SlotId, SlotRole)
import Shared.Types.Agent.State as PublicState
import StetsonHelper (GetHandler, jsonResponse)

stats :: SlotId -> SlotRole -> GetHandler PublicState.Egest
stats slotId slotRole =
  jsonResponse $ (map Just) $ EgestInstance.currentStats $ EgestKey slotId slotRole
