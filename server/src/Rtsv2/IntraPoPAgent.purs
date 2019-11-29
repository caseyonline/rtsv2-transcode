module Rtsv2.IntraPoPAgent
  ( startLink
  , Config
  ) where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe, fromMaybe')
import Effect (Effect)
import Erl.Atom (Atom, atom)
import Erl.Data.List (List, nil, (:))
import Foreign (Foreign)
import Gproc as Gproc
import Ip as Ip
import Logger as Logger
import Os (getEnv)
import Pinto (ServerName(..), StartLinkResult)
import Pinto.Gen (CallResult(..))
import Pinto.Gen as Gen
import Prim.Row (class Nub)
import Record as Record
import Rtsv2.PoPDefinition as PoPDefinition
import Serf (Ip(..), IpAndPort)
import Serf as Serf
import Shared.Agents as Agents
import Shared.Utils (lazyCrashIfMissing, strToIp)

type State
  = {}

type Config
  = { bindPort :: Int
    , rpcPort :: Int
    }

serverName :: ServerName State Unit
serverName = Local "intraPopAgent"

startLink :: Config -> Effect StartLinkResult
startLink args = Gen.startLink serverName (init args) Gen.defaultHandleInfo

init :: Config -> Effect State
init config =
  do
    seeds <- PoPDefinition.getSeeds :: Effect (List String)
    maybeIface <- getEnv "IFACE"
    maybeIfaceIp <- getIp maybeIface

    let
      rpcBindIp :: Ip
      rpcBindIp = fromMaybe' (lazyCrashIfMissing ("Invalid RPC Interface: " <> (show maybeIface))) maybeIfaceIp
      serfRpcAddress = { ip: show rpcBindIp
                       , port: config.rpcPort
                       }
      seedAddresses = map (\s -> {ip : s
                                 , port: config.bindPort
                                 }) seeds

    _ <- logInfo "Intra-PoP Agent Starting" { config : config
                                          , seeds : seedAddresses}

    result <- Serf.join serfRpcAddress seedAddresses true

    _ <- logInfo "Serf said " {result: result}

    pure $ {}
  where
    getIp :: Maybe String -> Effect (Maybe Ip)
    getIp Nothing = pure $ Nothing
    getIp (Just str) = Ip.getInterfaceIp str


logInfo :: forall a b. Nub (domain :: List Atom | a) b =>  String -> Record a -> Effect Foreign
logInfo msg metaData =
  Logger.info msg (Record.merge {domain : ((atom (show Agents.IntraPoPAgent)): nil)} {misc: metaData})
