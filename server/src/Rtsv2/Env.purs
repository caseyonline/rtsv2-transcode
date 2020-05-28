module Rtsv2.Env
  ( hostname
  , publicInterfaceName
  , publicInterfaceIp
  , supportInterfaceName
  , supportInterfaceIp
  , systemInterfaceName
  , systemInterfaceIp
  , isProxied
  ) where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe')
import Effect (Effect)
import Ip as Ip
import Os (getEnv)
import Serf (Ip)
import Shared.Utils (lazyCrashIfMissing)

hostname :: Effect String
hostname = mandatoryOsEnv "HOSTNAME"

publicInterfaceName :: Effect String
publicInterfaceName = mandatoryOsEnv "PUBLIC_IFACE"

supportInterfaceName :: Effect String
supportInterfaceName = mandatoryOsEnv "SUPPORT_IFACE"

systemInterfaceName :: Effect String
systemInterfaceName = mandatoryOsEnv "SYSTEM_IFACE"

publicInterfaceIp :: Effect Ip
publicInterfaceIp = mandatoryInterfaceIp =<< publicInterfaceName

supportInterfaceIp :: Effect Ip
supportInterfaceIp = mandatoryInterfaceIp =<< supportInterfaceName

systemInterfaceIp :: Effect Ip
systemInterfaceIp = mandatoryInterfaceIp =<< systemInterfaceName

isProxied :: Effect Boolean
isProxied = fromMaybe' (lazyCrashIfMissing ("Invalid isProxied environment variable")) <$> parse <$> mandatoryOsEnv "IS_PROXIED"
  where
    parse "true" = Just true
    parse "false" = Just false
    parse _ = Nothing

mandatoryOsEnv :: String -> Effect String
mandatoryOsEnv env = do
  maybeValue <- getEnv env
  pure $ fromMaybe' (lazyCrashIfMissing ("Missing environment variable: " <> env)) maybeValue

mandatoryInterfaceIp :: String -> Effect Ip
mandatoryInterfaceIp name =
  do
    maybeIfaceIp <- getIp name

    pure $ fromMaybe' (lazyCrashIfMissing ("Invalid Interface Name: " <> name)) maybeIfaceIp
  where
    getIp :: String -> Effect (Maybe Ip)
    getIp str = Ip.getInterfaceIp str
