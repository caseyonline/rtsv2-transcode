-- -*- psc-ide-codegen: ("erl") -*-
module Os
       (
         getEnv
       )
       where

import Data.Maybe (Maybe(..))
import Effect (Effect)

foreign import getEnv_ :: Maybe String -> (String -> Maybe String) -> String -> Effect (Maybe String)

getEnv :: String -> Effect (Maybe String)
getEnv = getEnv_ Nothing Just
