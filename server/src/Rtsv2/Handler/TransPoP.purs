module Rtsv2.Handler.TransPoP
       (
         timedRoutes
       ) where


import Prelude

import Erl.Data.List (List)
import Rtsv2.Agents.TransPoP as TransPoP
import Shared.Types (PoPName)
import Shared.Types.Agent.State as PublicState
import StetsonHelper (GenericStetsonGet, genericGet)

timedRoutes :: PoPName -> GenericStetsonGet (PublicState.TimedPoPRoutes List)
timedRoutes = genericGet <<< TransPoP.getTimedRoutesTo
