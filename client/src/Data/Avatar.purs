module Rtsv2App.Data.Avatar
  ( Avatar -- constructor not exported
  , parse
  , toString
  , toStringWithDefault
  ) where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Simple.JSON (class ReadForeign, class WriteForeign)

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------
newtype Avatar = Avatar String

derive instance genericAvatar :: Generic Avatar _
derive instance eqAvatar :: Eq Avatar

derive newtype instance encodeJsonAvatar :: ReadForeign Avatar
derive newtype instance decodeJsonAvatar :: WriteForeign Avatar

instance showAvatar :: Show Avatar where
  show = genericShow


-------------------------------------------------------------------------------
-- Helper functions
-------------------------------------------------------------------------------
parse :: String -> Maybe Avatar
parse "" = Nothing
parse str = Just (Avatar str)

toString :: Avatar -> String
toString (Avatar str) = str

toStringWithDefault :: Maybe Avatar -> String
toStringWithDefault (Just av) = toString av
toStringWithDefault Nothing =
  "https://static.productionready.io/images/smiley-cyrus.jpg"
