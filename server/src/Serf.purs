module Serf
       ( strToIp
       , join
       , event
       , stream
       , messageMapper
       , Ip(..), IpAndPort, ApiError(..), Message(..), SerfResult(..)
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
import Foreign (Foreign)

type SerfResult a = Either ApiError a

data Message a = MemberAlive
               | MemberLeaving
               | MemberLeft
               | MemberFailed
               | UserEvent String Int Boolean a

foreign import joinImpl :: (ApiError -> (SerfResult Int)) -> (Int -> SerfResult Int) -> IpAndPort -> List IpAndPort -> Boolean ->  Effect (SerfResult Int)
foreign import eventImpl :: forall a. (ApiError -> (SerfResult Unit)) -> (SerfResult Unit) -> IpAndPort -> String -> a -> Boolean ->  Effect (SerfResult Unit)
foreign import streamImpl :: (ApiError -> (SerfResult Unit)) -> (SerfResult Unit) -> IpAndPort -> Effect (SerfResult Unit)
foreign import messageMapperImpl :: forall a. Foreign -> Message a

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
join = joinImpl Left Right

event :: forall a. IpAndPort -> String -> a -> Boolean ->  Effect (SerfResult Unit)
event = eventImpl Left (Right unit)

stream :: IpAndPort -> Effect (SerfResult Unit)
stream = streamImpl Left (Right unit)

messageMapper :: forall a. Foreign -> Message a
messageMapper = messageMapperImpl
