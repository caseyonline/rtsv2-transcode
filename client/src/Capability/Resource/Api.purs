module Rtsv2App.Capability.Resource.Api where

import Prelude

import Data.Either (Either)
import Data.Maybe (Maybe)
import Halogen (HalogenM, lift)
import Rtsv2App.Component.Utils (SlotDetailsArgs, PoPAggrSelectedInfo)
import Shared.Rtsv2.Types (PoPName, ServerAddress)
import Shared.Rtsv2.Agent.State (IntraPoP, PoPDefinition, TimedPoPRoutes, IngestAggregator)

class Monad m <= ManageApi m where
  getAggregatorDetails :: SlotDetailsArgs -> m (Either String (IngestAggregator Array))
  getTimedRoutes       :: PoPAggrSelectedInfo -> PoPName -> m (Either String (TimedPoPRoutes Array))
  getPoPdefinition     :: m (Either String (PoPDefinition Array))
  getServerState       :: Maybe ServerAddress -> m (Either String (IntraPoP Array))

instance manageApiHalogenM :: ManageApi m => ManageApi (HalogenM st act slots msg m) where
  getAggregatorDetails = lift <<< getAggregatorDetails
  getTimedRoutes   a b = lift $ getTimedRoutes a b
  getPoPdefinition     = lift getPoPdefinition
  getServerState       = lift <<< getServerState
