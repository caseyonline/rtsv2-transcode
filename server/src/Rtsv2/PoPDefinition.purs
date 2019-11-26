module Rtsv2.PoPDefinition
       (startLink
       , init
       , serverName
       , State
       , StartArgs
       ) where

import Prelude

import Effect (Effect)
import Erl.Data.List (List)
import Pinto (ServerName(..), StartLinkResult)
import Pinto.Gen as Gen

type StartArgs = {
                 }

newtype State = State {}

type PoP = { name :: String
           , servers :: List String
           }

type Region = { name :: String
              , pops :: List PoP
              }

serverName :: ServerName State
serverName = ServerName "popDefinition"

startLink :: StartArgs -> Effect StartLinkResult
startLink args =
  Gen.startLink serverName $ init args

init :: StartArgs -> Effect State
init args = do
  pure $ State {}
