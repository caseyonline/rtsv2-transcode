module Rtsv2.EnvConfig
  ( privateInterfaceName
  , privateInterfaceIp
  ) where

import Prelude

import Data.Maybe (Maybe, fromMaybe')
import Effect (Effect)
import Ip as Ip
import Os (getEnv)
import Serf (Ip)
import Shared.Utils (lazyCrashIfMissing)

privateInterfaceName :: Effect String
privateInterfaceName =
  do
    maybeIface <- getEnv "PRIVATE_IFACE"
    pure $ fromMaybe' (lazyCrashIfMissing ("Invalid Private Interface Name: " <> (show maybeIface))) maybeIface

privateInterfaceIp :: Effect Ip
privateInterfaceIp =
  do
    name <- privateInterfaceName
    maybeIfaceIp <- getIp name

    pure $ fromMaybe' (lazyCrashIfMissing ("Invalid RPC Interface: " <> name)) maybeIfaceIp
  where
    getIp :: String -> Effect (Maybe Ip)
    getIp str = Ip.getInterfaceIp str
