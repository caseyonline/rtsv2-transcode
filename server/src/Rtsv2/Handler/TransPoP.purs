module Rtsv2.Handler.TransPoP
       (
         timedRoutes
       ) where


import Prelude

import Data.Maybe (Maybe(..))
import Erl.Data.List (List)
import Rtsv2.Agents.TransPoP as TransPoP
import Shared.Types (PoPName)
import Shared.Types.Agent.State as PublicState
import StetsonHelper (GetResponse, jsonResponse)

timedRoutes :: PoPName -> GetResponse (PublicState.TimedPoPRoutes List)
timedRoutes = jsonResponse <<< (map Just) <<< TransPoP.getTimedRoutesTo
