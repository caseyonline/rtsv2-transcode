module Rtsv2App.Capability.Navigate where

import Prelude

import Control.Monad.Trans.Class (lift)
import Rtsv2App.Data.Route (Route)
import Halogen (HalogenM)

-------------------------------------------------------------------------------
-- | The `logout`function should clear any information associated with
-- | the user from the app and browser before redirecting them to the homepage.
-------------------------------------------------------------------------------
class Monad m <= Navigate m where
  navigate :: Route -> m Unit
  logout :: m Unit

-- | This instance lets us avoid having to use `lift` when we use these functions in a component.
instance navigateHalogenM :: Navigate m => Navigate (HalogenM st act slots msg m) where
  navigate = lift <<< navigate
  logout = lift logout
