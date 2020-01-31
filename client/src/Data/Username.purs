module Rtsv2App.Data.Username
  ( Username -- constructor not exported
  , parse
  , toString
  ) where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Simple.JSON (class ReadForeign, class WriteForeign)


-------------------------------------------------------------------------------
-- User Types
-------------------------------------------------------------------------------
newtype Username = Username String

derive instance genericUsername :: Generic Username _
derive instance eqUsername :: Eq Username
derive instance ordUsername :: Ord Username

derive newtype instance readForeignUsername  :: ReadForeign Username
derive newtype instance writeForeignUsername :: WriteForeign Username

instance showUsername :: Show Username where
  show = genericShow

parse :: String -> Maybe Username
parse "" = Nothing
parse str = Just (Username str)

toString :: Username -> String
toString (Username str) = str
