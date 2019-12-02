module Serf
       ( strToIp
       , join
       , event
       , stream
       , messageMapper
       , Ip(..), IpAndPort(..), ApiError(..), Message(..)
       )
       where

import Prelude

import Data.Either (Either(..))
import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), split)
import Data.Traversable (sequence)
import Effect (Effect)
import Erl.Data.Binary (Binary)
import Erl.Data.List (List)
import Foreign (Foreign)

type SerfResult a = Either ApiError a

data Message = MemberAlive
             | MemberLeaving
             | MemberLeft
             | MemberFailed
             | UserEvent String Int Boolean Binary

foreign import joinImpl :: IpAndPort -> List IpAndPort -> Boolean ->  Effect (SerfResult Int)
foreign import eventImpl :: forall a. IpAndPort -> String -> a -> Boolean ->  Effect (SerfResult Unit)
foreign import streamImpl :: forall a. (ApiError -> (SerfResult Unit)) -> (SerfResult Unit) -> IpAndPort -> Effect (SerfResult Unit)
foreign import messageMapperImpl :: Foreign -> Message

data Ip = Ipv4 Int Int Int Int

instance showAgent :: Show Ip where
  show (Ipv4 a b c d) = (show a) <> "." <> (show b) <> "." <> (show c) <> "." <> (show d)

data ApiError = SerfError String
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


join :: IpAndPort -> List IpAndPort -> Boolean ->  Effect (SerfResult Int)
join = joinImpl

event :: forall a. IpAndPort -> String -> a -> Boolean ->  Effect (SerfResult Unit)
event = eventImpl

stream :: IpAndPort -> Effect (SerfResult Unit)
stream = streamImpl Left (Right unit)

messageMapper :: Foreign -> Message
messageMapper = messageMapperImpl
