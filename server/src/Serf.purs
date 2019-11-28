module Serf
       where

import Prelude

import Data.Either (Either(..))
import Effect (Effect)
import Erl.Data.List (List)



foreign import joinImpl :: IpAndPort -> List IpAndPort -> Boolean ->  Effect (Either SerfApiError Int)

data Ip = Ipv4 Int Int Int Int

data SerfApiError = SerfError String
                  | NetworkError

type IpAndPort = { ip :: String
                 , port :: Int
                 }

join :: IpAndPort -> List IpAndPort -> Boolean ->  Effect (Either SerfApiError Int)
join = joinImpl
