module Shared.Types ( ServerAddress(..)
                    ) where

import Prelude

import Data.Newtype (class Newtype)
import Simple.JSON as JSON

newtype ServerAddress = ServerAddress String

derive instance newtypeServerAddress :: Newtype ServerAddress _
derive newtype instance eqServerAddress :: Eq ServerAddress
derive newtype instance ordServerAddress :: Ord ServerAddress
derive newtype instance showServerAddress :: Show ServerAddress
derive newtype instance readForeignServerAddress :: JSON.ReadForeign ServerAddress
derive newtype instance writeForeignServerAddress :: JSON.WriteForeign ServerAddress
