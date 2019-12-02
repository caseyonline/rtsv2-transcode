module Ip
       (
         getInterfaceIp
       )
       where

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Serf (Ip)

foreign import getInterfaceIp_ :: Maybe Ip -> (Ip -> Maybe Ip) -> String -> Effect (Maybe Ip)

getInterfaceIp :: String -> Effect (Maybe Ip)
getInterfaceIp = getInterfaceIp_ Nothing Just
