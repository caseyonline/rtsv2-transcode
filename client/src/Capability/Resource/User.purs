module Rtsv2App.Capability.Resource.User where

import Prelude

import Data.Maybe (Maybe)
import Halogen (HalogenM, lift)
import Rtsv2App.Api.Request (AuthFieldsRep, LoginFields, RegisterFields)
import Rtsv2App.Data.Profile (Profile, ProfileRep, ProfileWithEmail, Author)
import Rtsv2App.Data.Username (Username)
import Type.Row (type (+))

type UpdateProfileFields = { | ProfileRep + AuthFieldsRep Maybe () }

class Monad m <= ManageUser m where
  loginUser :: LoginFields -> m (Maybe Profile)
  registerUser :: RegisterFields -> m (Maybe Profile)
  getCurrentUser :: m (Maybe ProfileWithEmail)
  getAuthor :: Username -> m (Maybe Author)
  updateUser :: UpdateProfileFields -> m Unit

instance manageUserHalogenM :: ManageUser m => ManageUser (HalogenM st act slots msg m) where
  loginUser = lift <<< loginUser
  registerUser = lift <<< registerUser
  getCurrentUser = lift getCurrentUser
  getAuthor = lift <<< getAuthor
  updateUser = lift <<< updateUser
