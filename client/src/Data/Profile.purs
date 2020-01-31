module Rtsv2App.Data.Profile where

import Data.Lens (Lens')
import Data.Lens.Record (prop)
import Data.Maybe (Maybe)
import Data.Symbol (SProxy(..))
import Rtsv2App.Data.Avatar (Avatar)
import Rtsv2App.Data.Email (Email)
import Rtsv2App.Data.Username (Username)


type ProfileRep row =
  ( username :: Username
  , bio :: Maybe String
  , image :: Maybe Avatar
  | row
  )

-- | The `Profile` type consists only of three core fields: the username, biography, and avatar.
type Profile = { | ProfileRep () }
type ProfileRes = { user :: Profile }

-- | The `ProfileEmail` type extends the `Profile` fields with an additional `Email` type.
type ProfileEmail = { | ProfileRep (email :: Email) }
type ProfileEmailRes = { user :: ProfileEmail }

-- | A lens for a username field within a record
_username :: forall r. Lens' { username :: Username | r } Username
_username = prop (SProxy :: SProxy "username")

-- | A lens for a bio field within a record
_bio :: forall r. Lens' { bio :: Maybe String | r } (Maybe String)
_bio = prop (SProxy :: SProxy "bio")

-- | A lens for an image field within a record
_avatar :: forall r. Lens' { avatar :: Maybe Avatar | r } (Maybe Avatar)
_avatar = prop (SProxy :: SProxy "avatar")
