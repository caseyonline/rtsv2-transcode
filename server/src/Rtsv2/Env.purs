module Rtsv2.Env
  ( hostname
  , publicInterfaceName
  , publicInterfaceIp
  , privateInterfaceName
  , privateInterfaceIp
  ) where

import Prelude

import Data.Maybe (Maybe, fromMaybe')
import Effect (Effect)
import Ip as Ip
import Os (getEnv)
import Serf (Ip)
import Shared.Utils (lazyCrashIfMissing)

hostname :: Effect String
hostname =
  do
    maybeHostName <- getEnv "HOSTNAME"
    pure $ fromMaybe' (lazyCrashIfMissing "No Hostname available") maybeHostName

publicInterfaceName :: Effect String
publicInterfaceName =
  do
    maybeIface <- getEnv "PUBLIC_IFACE"
    pure $ fromMaybe' (lazyCrashIfMissing ("Invalid Public Interface Name: " <> (show maybeIface))) maybeIface

publicInterfaceIp :: Effect Ip
publicInterfaceIp =
  do
    name <- publicInterfaceName
    maybeIfaceIp <- getIp name

    pure $ fromMaybe' (lazyCrashIfMissing ("Invalid Public Interface Name: " <> name)) maybeIfaceIp
  where
    getIp :: String -> Effect (Maybe Ip)
    getIp str = Ip.getInterfaceIp str

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

    pure $ fromMaybe' (lazyCrashIfMissing ("Invalid Private Interface Name: " <> name)) maybeIfaceIp
  where
    getIp :: String -> Effect (Maybe Ip)
    getIp str = Ip.getInterfaceIp str

