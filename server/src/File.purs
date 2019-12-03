-- -*- psc-ide-codegen: ("erl") -*-
module File
       (
         readBinaryFile
       , readUtf8File
       )
       where

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Erl.Data.Binary (Binary)

foreign import readFile_ :: forall a. Maybe a -> (a -> Maybe a) -> String -> Effect (Maybe a)

readBinaryFile :: String -> Effect (Maybe Binary)
readBinaryFile = readFile_ Nothing Just

readUtf8File :: String -> Effect (Maybe String)
readUtf8File = readFile_ Nothing Just
