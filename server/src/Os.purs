-- -*- psc-ide-codegen: ("erl") -*-
module Os
       ( getEnv
       , osCmd
       , privCmd
       )
       where

import Data.Maybe (Maybe(..))
import Effect (Effect)

foreign import getEnv_ :: Maybe String -> (String -> Maybe String) -> String -> Effect (Maybe String)

getEnv :: String -> Effect (Maybe String)
getEnv = getEnv_ Nothing Just

foreign import osCmd :: String -> Effect String
foreign import privCmd :: String -> Effect String
