module Rtsv2.Handler.TransPoP
       (
         timedRoutes
       ) where


import Erl.Data.List (List)
import Rtsv2.Agents.TransPoP as TransPoP
import Shared.Types.Agent.State as PublicState
import StetsonHelper (GenericStetsonGetBySlotId, genericGetByPoPName)


timedRoutes :: GenericStetsonGetBySlotId (PublicState.TimedPoPRoutes List)
timedRoutes = genericGetByPoPName TransPoP.getTimedRoutesTo
