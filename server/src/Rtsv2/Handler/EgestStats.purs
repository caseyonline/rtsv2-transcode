module Rtsv2.Handler.EgestStats
       (
         stats
       ) where

import Rtsv2.Agents.EgestInstance as EgestInstance
import Stetson (StetsonHandler)

import Rtsv2.Handler.Status as Status
import Shared.Types.Agent.State as PublicState

stats :: StetsonHandler (Status.StatusState PublicState.Egest)
stats = Status.status EgestInstance.currentStats
