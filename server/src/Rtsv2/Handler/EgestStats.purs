module Rtsv2.Handler.EgestStats
       (
         stats
       ) where

import Rtsv2.Agents.EgestInstance as EgestInstance
import Shared.Types.Agent.State as PublicState
import StetsonHelper (GenericStetsonGetByStreamId, genericGetByStreamId)

stats :: GenericStetsonGetByStreamId PublicState.Egest
stats = genericGetByStreamId EgestInstance.currentStats
