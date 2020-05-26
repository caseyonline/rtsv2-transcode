module Rtsv2.Handler.TransPoP
       (
         timedRoutes
       , timedRoutesForPoP
       ) where


import Prelude

import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Erl.Data.List (List)
import Rtsv2.Agents.TransPoP as TransPoP
import Shared.Rtsv2.Agent.State as PublicState
import Shared.Rtsv2.JsonLd as JsonLd
import Shared.Rtsv2.Types (PoPName)
import StetsonHelper (GetHandler, jsonResponse)

timedRoutes :: GetHandler (List (PublicState.TimedPoPNeighbour List))
timedRoutes =
  jsonResponse $
    (\(Tuple server neighours) ->
        Just <$> (traverse (JsonLd.timedRouteNeighbourNode server) neighours)
    ) =<< TransPoP.getNeighbours


timedRoutesForPoP :: PoPName -> GetHandler (PublicState.TimedPoPRoutes List)
timedRoutesForPoP = jsonResponse <<< (map Just) <<< TransPoP.getTimedRoutesTo
