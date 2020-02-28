module Rtsv2.Handler.PoPDefinition
       ( popDefinition
       ) where

import Erl.Data.List (List)
import Rtsv2.PoPDefinition as PoPDefinition
import Shared.Types.Agent.State as PublicState
import StetsonHelper (GenericStetsonGet, jsonResponse)

popDefinition :: GenericStetsonGet (PublicState.PoPDefinition List)
popDefinition = jsonResponse PoPDefinition.getPublicPoPDefinition
