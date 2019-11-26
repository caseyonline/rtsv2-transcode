module Rtsv2.IntraPoPAgent where

import Prelude

import Effect (Effect)
import Erl.Data.List (List, nil, (:))
import Erl.Atom (Atom, atom)
import Foreign (Foreign)
import Gproc as Gproc
import Logger as Logger
import Pinto (ServerName(..), StartLinkResult)
import Pinto.Gen (CallResult(..))
import Pinto.Gen as Gen
import Prim.Row (class Nub)
import Record as Record
import Shared.Agents as Agents
import Serf (Ip(..), IpAndPort)
import Serf as Serf

type State
  = {}

type IntraPoPAgentStartArgs
  = { rpcPort :: Int
    }

serverName :: ServerName State
serverName = ServerName "intraPopAgent"

startLink :: IntraPoPAgentStartArgs -> Effect StartLinkResult
startLink args = Gen.startLink serverName $ init args

init :: IntraPoPAgentStartArgs -> Effect State
init config = do
  let
    serfRpcAddress = { ip: Ipv4 127 0 0 1
                     , port: config.rpcPort
                     }

  _ <- logInfo "Intra-PoP Agent Starting" config

--  result <- Serf.join serfRpcAddress defaultSeeds true

  pure $ {}

logInfo :: forall a b. Nub (domain :: List Atom | a) b =>  String -> Record a -> Effect Foreign
logInfo msg metaData =
  Logger.info msg (Record.merge {domain : ((atom (show Agents.IntraPoPAgent)): nil)} {misc: metaData})
