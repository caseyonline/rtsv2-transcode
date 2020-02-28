module Rtsv2.Handler.TransPoP
       (
         timedRoutes
       ) where


import Prelude

import Erl.Data.List (List)
import Rtsv2.Agents.TransPoP as TransPoP
import Shared.Types (PoPName)
import Shared.Types.Agent.State as PublicState
import StetsonHelper (GenericStetsonGet, jsonResponse)

timedRoutes :: PoPName -> GenericStetsonGet (PublicState.TimedPoPRoutes List)
timedRoutes = jsonResponse <<< TransPoP.getTimedRoutesTo
