module Rtsv2.Env
  ( hostname
  , publicListenIp
  , supportListenIp
  , systemListenIp
  , intraSerfIp
  , transSerfIp
  , isProxied
  ) where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe')
import Effect (Effect)
import Ip as Ip
import Os (getEnv)
import Serf (Ip)
import Shared.Utils (lazyCrashIfMissing)

------------------------------------------------------------------------------
-- API
------------------------------------------------------------------------------
hostname :: Effect String
hostname = mandatoryOsEnv "HOSTNAME"

publicListenIp :: Effect Ip
publicListenIp = mandatoryInterfaceIp =<< publicInterfaceName

supportListenIp :: Effect Ip
supportListenIp = mandatoryInterfaceIp =<< supportInterfaceName

systemListenIp :: Effect Ip
systemListenIp = mandatoryInterfaceIp =<< systemInterfaceName

intraSerfIp :: Effect Ip
intraSerfIp = mandatoryInterfaceIp =<< intraSerfInterfaceName

transSerfIp :: Effect Ip
transSerfIp = mandatoryInterfaceIp =<< transSerfInterfaceName

isProxied :: Effect Boolean
isProxied = fromMaybe' (lazyCrashIfMissing ("Invalid isProxied environment variable")) <$> parse <$> mandatoryOsEnv "IS_PROXIED"
  where
    parse "true" = Just true
    parse "false" = Just false
    parse _ = Nothing

------------------------------------------------------------------------------
-- Internals
------------------------------------------------------------------------------
publicInterfaceName :: Effect String
publicInterfaceName = mandatoryOsEnv "PUBLIC_IFACE"

supportInterfaceName :: Effect String
supportInterfaceName = mandatoryOsEnv "SUPPORT_IFACE"

systemInterfaceName :: Effect String
systemInterfaceName = mandatoryOsEnv "SYSTEM_IFACE"

intraSerfInterfaceName :: Effect String
intraSerfInterfaceName = mandatoryOsEnv "INTRA_SERF_IFACE"

transSerfInterfaceName :: Effect String
transSerfInterfaceName = mandatoryOsEnv "TRANS_SERF_IFACE"

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
