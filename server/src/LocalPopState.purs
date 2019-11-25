module LocalPopState where

import Prelude

import Data.Either (Either(..))
import Debug.Trace (spy)
import Effect (Effect)
import Erl.Data.List (List, nil, (:))
import Pinto (ServerName(..), StartLinkResult)
import Pinto.Gen (CallResult(..))
import Pinto.Gen as Gen
import Serf (Ip(..), IpAndPort)
import Serf as Serf

type State
  = {}

type PopStateStartArgs
  = { serfRpcAddress :: IpAndPort
    }

serverName :: ServerName State
serverName = ServerName "globalState"

defaultStartArgs :: PopStateStartArgs
defaultStartArgs =
  { serfRpcAddress:
    { ip: Ipv4 127 0 0 1
    , port: 7373
    }
  }

defaultSeeds :: List IpAndPort
defaultSeeds =
  ( { ip: Ipv4 127 0 0 1, port: 7948 }
  : { ip: Ipv4 127 0 0 1, port: 7947 }
  : nil
  )

startLink :: PopStateStartArgs -> Effect StartLinkResult
startLink args = Gen.startLink serverName $ init args

isStreamAvailable :: String -> Effect Boolean
isStreamAvailable s = Gen.call serverName \state -> CallReply false state

init :: PopStateStartArgs -> Effect State
init { serfRpcAddress } = do
  -- What starts the local serf instance?  systemd etc or this code?
  -- Join the local SWIM network and stream events from it
  result <- Serf.join serfRpcAddress defaultSeeds true
  let _ =
        case result of
          Left error ->
            let _ = spy "error" error
            in unit
          Right foo ->
            let _ = spy "right" foo
            in unit
  pure $ {}
