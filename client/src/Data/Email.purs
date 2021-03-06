module Rtsv2App.Data.Email where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Newtype (class Newtype)
import Simple.JSON (class ReadForeign, class WriteForeign)


-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------
newtype Email = Email String

derive instance newtypeEmail :: Newtype Email _
derive instance genericEmail :: Generic Email _
derive instance eqEmail :: Eq Email
derive instance ordEmail :: Ord Email

derive newtype instance readForeignEmail :: ReadForeign Email
derive newtype instance writeForeignEmail :: WriteForeign Email

instance showEmail :: Show Email where
  show = genericShow
