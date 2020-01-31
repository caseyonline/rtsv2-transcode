module Rtsv2App.Capability.Resource.Stats where

import Prelude

import Data.Maybe (Maybe)
import Halogen (HalogenM, lift)
import Shared.Types (PoPName)
import Shared.Types.Agent.State (TimedPoPRoutes)

class Monad m <= ManageStats m where
  postTimedRoutes :: PoPName -> m (Maybe TimedPoPRoutes)

instance manageUserHalogenM :: ManageStats m => ManageStats (HalogenM st act slots msg m) where
  postTimedRoutes = lift <<< postTimedRoutes
