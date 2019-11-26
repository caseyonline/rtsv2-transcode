module Rtsv2.PoPDefinition
       (startLink
       , init
       , serverName
       , State
       , Config
       ) where

import Prelude

import Effect (Effect)
import Erl.Data.List (List)
import File as File
import Logger as Logger
import Pinto (ServerName(..), StartLinkResult)
import Pinto.Gen (CastResult(..))
import Pinto.Gen as Gen
import Pinto.Gen as PintoGen
import Pinto.Timer as Timer

type Config = { popDefinitionFile :: String
              }

type State =  { config :: Config
              }

type PoP = { name :: String
           , servers :: List String
           }

type Region = { name :: String
              , pops :: List PoP
              }

serverName :: ServerName State
serverName = ServerName "popDefinition"

startLink :: Config -> Effect StartLinkResult
startLink args =
  Gen.startLink serverName $ init args

init :: Config -> Effect State
init config = do
  _ <- Timer.sendAfter 1000 handleTick
  pure $ {config : config}


handleTick :: Effect Unit
handleTick = PintoGen.doCast serverName \state -> do

  file <- File.readFile state.config.popDefinitionFile
  _ <- Logger.info "ping" {misc: file}
  _ <- Timer.sendAfter 1000 handleTick
  pure $ CastNoReply state
