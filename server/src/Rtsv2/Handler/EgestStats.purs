module Rtsv2.Handler.EgestStats
       (
         stats
       ) where

import Prelude

import Data.Maybe (Maybe(..))
import Erl.Data.List (List)
import Rtsv2.Agents.EgestInstance as EgestInstance
import Shared.Rtsv2.Agent.State as PublicState
import Shared.Rtsv2.JsonLd as JsonLd
import Shared.Rtsv2.Stream (EgestKey(..), SlotId, SlotRole)
import Shared.Rtsv2.Types (Server)
import StetsonHelper (GetHandler, jsonResponse)

stats :: Server -> SlotId -> SlotRole -> GetHandler (PublicState.Egest List)
stats server slotId slotRole =
  jsonResponse $ (map Just) <$> (map (JsonLd.egestStatsNode slotId slotRole server)) <$> EgestInstance.currentStats $ EgestKey slotId slotRole
