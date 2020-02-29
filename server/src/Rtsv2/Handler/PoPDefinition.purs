module Rtsv2.Handler.PoPDefinition
       ( popDefinition
       ) where

import Prelude

import Data.Maybe (Maybe(..))
import Erl.Data.List (List)
import Rtsv2.PoPDefinition as PoPDefinition
import Shared.Types.Agent.State as PublicState
import StetsonHelper (GetHandler, jsonResponse)

popDefinition :: GetHandler (PublicState.PoPDefinition List)
popDefinition = jsonResponse $ Just <$> PoPDefinition.getPublicPoPDefinition
