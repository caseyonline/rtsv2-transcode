module Rtsv2.Handler.EgestStats
       (
         stats
       ) where

import PintoHelper (GenericStatusState, genericStatus)
import Rtsv2.Agents.EgestInstance as EgestInstance
import Shared.Types.Agent.State as PublicState
import Stetson (StetsonHandler)

stats :: StetsonHandler (GenericStatusState PublicState.Egest)
stats = genericStatus EgestInstance.currentStats
