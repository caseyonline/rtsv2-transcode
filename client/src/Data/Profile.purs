module Rtsv2App.Data.Profile where

import Prelude

import Rtsv2App.Data.Avatar (Avatar)
import Rtsv2App.Data.Email (Email)
import Rtsv2App.Data.Username (Username)
import Rtsv2App.Data.Utils (decodeAt)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Decode.Struct.Tolerant as Tolerant
import Data.Argonaut.Decode.Struct.Tolerant.GDecodeJson (class GDecodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Either (Either)
import Data.Lens (Lens')
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Struct.RenameMany (rrenameMany)
import Data.Symbol (SProxy(..))
import Record.Builder (Builder)
import Type.Row (class Lacks)
import Type.RowList (class RowToList, Nil)

data Relation
  = Following
  | NotFollowing
  | You

derive instance eqRelation :: Eq Relation

instance decodeJsonFollowStatus :: DecodeJson Relation where
  decodeJson = decodeJson >=> if _ then pure Following else pure NotFollowing 

instance encodeJsonFollowStatus :: EncodeJson Relation where
  encodeJson Following = encodeJson true
  encodeJson _ = encodeJson false

type ProfileRep row =
  ( username :: Username
  , bio :: Maybe String
  , image :: Maybe Avatar
  | row
  )

-- | The `Profile` type consists only of three core fields: the username, biography, and avatar.
type Profile = { | ProfileRep () }

-- | The `ProfileWithEmail` type extends the `Profile` fields with an additional `Email` type.
type ProfileWithEmail = { | ProfileRep (email :: Email) }

-- | The `Author` type extends the `Profile` fields with an additional `Relation` type.
type Author = { | ProfileRep (relation :: Relation) }

decodeAuthor :: Maybe Username -> Json -> Either String Author
decodeAuthor mbUsername =
  map (rrenameMany { following: SProxy :: SProxy "relation" })
    <<< Tolerant.decodeJsonWith { following: getRelation mbUsername }

getRelation :: forall r. Maybe Username -> Json -> { username :: Username | r } -> Either String Relation
getRelation mbUsername json record = case mbUsername of
  Just username | username == record.username -> pure You
  _ -> decodeJson json

-- | An example of its use can be found in 'Rtsv2App.Data.Article', where `decodeArticles` delegates
-- | to `decodeJsonWithAuthor` to determine an array of `ArticleWithMetadata` values.
decodeJsonWithAuthor
  :: forall l r
   . GDecodeJson Builder (Either String) Record Nil () l r
  => Lacks "author" r
  => RowToList r l
  => Maybe Username
  -> Json
  -> Either String { author :: Author | r }
decodeJsonWithAuthor u =
  Tolerant.decodeJsonPer { author: decodeAuthor u }

decodeProfileAuthor :: Maybe Username -> Json -> Either String Author
decodeProfileAuthor u = decodeAuthor u <=< decodeAt "profile"

-- | A lens for a username field within a record
_username :: forall r. Lens' { username :: Username | r } Username
_username = prop (SProxy :: SProxy "username")

-- | A lens for a bio field within a record
_bio :: forall r. Lens' { bio :: Maybe String | r } (Maybe String)
_bio = prop (SProxy :: SProxy "bio")

-- | A lens for an image field within a record
_avatar :: forall r. Lens' { avatar :: Maybe Avatar | r } (Maybe Avatar)
_avatar = prop (SProxy :: SProxy "avatar")

-- | A lens for a following field within a record
_relation :: forall r. Lens' { relation :: Relation | r } Relation
_relation = prop (SProxy :: SProxy "relation")
