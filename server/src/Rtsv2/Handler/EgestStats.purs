module Rtsv2.Handler.EgestStats
       (
         stats
       ) where

import Prelude

import Rtsv2.Agents.EgestInstance as EgestInstance
import Shared.Stream (EgestKey(..), SlotId)
import Shared.Types.Agent.State as PublicState
import StetsonHelper (GenericStetsonGet, genericGet)

stats :: SlotId -> GenericStetsonGet PublicState.Egest
stats = genericGet <<< EgestInstance.currentStats <<< EgestKey
