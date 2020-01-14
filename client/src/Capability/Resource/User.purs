-- | A capability representing the ability to manage users in our system. That includes logging in,
-- | registering, following / unfollowing, fetching an article's author, and more.
-- | 
-- | This capability lets us ignore the mechanics of managing a resource and focus on our
-- | business logic. For now our app implements this capability with a REST API, but we could 
-- | easily swap in a database, RPC, local filesystem, or something else without having to touch 
-- | any application code besides the application monad, `Rtsv2App.AppM`. In addition, we can test 
-- | our business logic  by mocking responses in our test monad instead of hitting the server.
-- |
-- | To learn more about why we use capabilities and this architecture, please see the guide:

module Rtsv2App.Capability.Resource.User where

import Prelude

import Rtsv2App.Api.Request (AuthFieldsRep, LoginFields, RegisterFields)
import Rtsv2App.Data.Profile (Profile, ProfileRep, ProfileWithEmail, Author)
import Rtsv2App.Data.Username (Username)
import Data.Maybe (Maybe)
import Halogen (HalogenM, lift)
import Type.Row (type (+))

-- | This type is a record made up of two row types: the fields that make up a profile, plus the
-- | fields used for authentication, like their email address and password. See the 
-- | `Rtsv2App.Data.Profile` module for more details.
type UpdateProfileFields = { | ProfileRep + AuthFieldsRep Maybe () }

-- | This capability represents the ability to manage users in our system. We support logging users
-- | in, and registering them, as well as reading information about various users and who follows 
-- | who.
-- | 
-- | We'll handle all the mechanics of making the request, decoding responses, handling errors, and 
-- | so on in the implementation.
class Monad m <= ManageUser m where
  loginUser :: LoginFields -> m (Maybe Profile)
  registerUser :: RegisterFields -> m (Maybe Profile)
  getCurrentUser :: m (Maybe ProfileWithEmail)
  getAuthor :: Username -> m (Maybe Author)
  updateUser :: UpdateProfileFields -> m Unit

-- | This instance lets us avoid having to use `lift` when we use these functions in a component.
instance manageUserHalogenM :: ManageUser m => ManageUser (HalogenM st act slots msg m) where
  loginUser = lift <<< loginUser
  registerUser = lift <<< registerUser
  getCurrentUser = lift getCurrentUser
  getAuthor = lift <<< getAuthor
  updateUser = lift <<< updateUser
