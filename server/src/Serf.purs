module Serf
       ( strToIp
       , join
       , event
       , Ip(..), IpAndPort(..), SerfApiError(..)
       )
       where

import Prelude

import Data.Either (Either(..))
import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), split)
import Data.Traversable (sequence)
import Effect (Effect)
import Erl.Data.List (List)

foreign import joinImpl :: IpAndPort -> List IpAndPort -> Boolean ->  Effect (Either SerfApiError Int)
foreign import eventImpl :: forall a. IpAndPort -> String -> a -> Boolean ->  Effect (Either SerfApiError Unit)

data Ip = Ipv4 Int Int Int Int

instance showAgent :: Show Ip where
  show (Ipv4 a b c d) = (show a) <> "." <> (show b) <> "." <> (show c) <> "." <> (show d)

data SerfApiError = SerfError String
                  | NetworkError

type IpAndPort = { ip :: String
                 , port :: Int
                 }

strToIp :: String -> Maybe Ip
strToIp str =
  str
  # split (Pattern ".")
  # map fromString
  # sequence
  # (\ip -> case ip of
        Just [a, b, c, d] -> Just (Ipv4 a b c d)
        _ -> Nothing
    )


join :: IpAndPort -> List IpAndPort -> Boolean ->  Effect (Either SerfApiError Int)
join = joinImpl


event :: forall a. IpAndPort -> String -> a -> Boolean ->  Effect (Either SerfApiError Unit)
event = eventImpl
