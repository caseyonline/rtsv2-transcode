module Rtsv2App.Capability.Resource.Api where

import Prelude

import Data.Either (Either)
import Data.Maybe (Maybe)
import Halogen (HalogenM, lift)
import Shared.Types (PoPName)
import Shared.Types.Agent.State (TimedPoPRoutes)

class Monad m <= ManageApi m where
  getTimedRoutes :: PoPName -> m (Either String TimedPoPRoutes)

instance manageApiHalogenM :: ManageApi m => ManageApi (HalogenM st act slots msg m) where
  getTimedRoutes = lift <<< getTimedRoutes
