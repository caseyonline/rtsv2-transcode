module Rtsv2App.Capability.Resource.Api where

import Prelude

import Data.Either (Either)
import Data.Maybe (Maybe)
import Halogen (HalogenM, lift)
import Shared.Types (PoPName, PoPSelectedInfo, ServerAddress)
import Shared.Types.Agent.State (IntraPoP, PoPDefinition, TimedPoPRoutes)

class Monad m <= ManageApi m where
  getTimedRoutes   :: PoPSelectedInfo -> PoPName -> m (Either String (TimedPoPRoutes Array))
  getPoPdefinition :: m (Either String (PoPDefinition Array))
  getPublicState   :: Maybe ServerAddress -> m (Either String (IntraPoP Array))

instance manageApiHalogenM :: ManageApi m => ManageApi (HalogenM st act slots msg m) where
  getTimedRoutes   a b = lift $ getTimedRoutes a b
  getPoPdefinition     = lift getPoPdefinition
  getPublicState       = lift <<< getPublicState
