module Rtsv2.Handler.TransPoP
       (
         timedRoutes
       ) where


import Rtsv2.Agents.TransPoP as TransPoP
import Shared.Types.Agent.State as PublicState
import StetsonHelper (GenericStetsonGetByStreamId, genericGetByPoPName)


timedRoutes :: GenericStetsonGetByStreamId PublicState.TimedPoPRoutes
timedRoutes = genericGetByPoPName TransPoP.getTimedRoutesTo
