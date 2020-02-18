module Rtsv2App.Capability.Resource.Api where

import Prelude

import Data.Either (Either)
import Halogen (HalogenM, lift)
import Rtsv2App.Env (OriginUrl)
import Shared.Types (PoPName)
import Shared.Types.Agent.State (PoPDefinition, TimedPoPRoutes, IntraPoP)

class Monad m <= ManageApi m where
  getTimedRoutes   :: PoPName -> m (Either String (TimedPoPRoutes Array))
  getPoPdefinition :: m (Either String (PoPDefinition Array))
  getPublicState      :: OriginUrl -> m (Either String (IntraPoP Array))

instance manageApiHalogenM :: ManageApi m => ManageApi (HalogenM st act slots msg m) where
  getTimedRoutes   = lift <<< getTimedRoutes
  getPoPdefinition = lift getPoPdefinition
  getPublicState      = lift <<< getPublicState
