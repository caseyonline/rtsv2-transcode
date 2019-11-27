module File
       (
         readFile
       )
       where

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Erl.Data.Binary (Binary)

foreign import readFile_ :: Maybe Binary -> (Binary -> Maybe Binary) -> String -> Effect (Maybe Binary)

readFile :: String -> Effect (Maybe Binary)
readFile = readFile_ Nothing Just
