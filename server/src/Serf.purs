module Serf
       ( strToIp
       , members
       , join
       , leave
       , event
       , stream
       , getCoordinate
       , calcRtt
       , messageMapper
       , Ip(..), IpAndPort, ApiError(..), SerfCoordinate, SerfMember, SerfMessage(..), SerfResult(..), LamportClock
       )
       where

import Prelude

import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Data.String (Pattern(..), split)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Erl.Atom (Atom)
import Erl.Data.List (List, zip)
import Erl.Data.Map (Map)
import Shared.Common (Milliseconds)
import Foreign (Foreign)
import Math (sqrt)

type SerfResult a = Either ApiError a

newtype LamportClock = LamportClock Int
derive instance eqLamportClock :: Eq LamportClock
derive instance ordLamportClock :: Ord LamportClock


type SerfMember = { name :: String
                  , addr :: String
                  , port :: Int
                  , tags :: Map String String
                  , status :: Atom
                  , protocolMin :: Int
                  , protocolMax :: Int
                  , protocolCur :: Int
                  , delegageMin :: Int
                  , delegageMax :: Int
                  , delegageCur :: Int
                  }

type SerfCoordinate = { adjustment :: Number
                      , error :: Number
                      , height :: Number
                      , vec :: List Number
                      }

data SerfMessage a = MemberAlive (List SerfMember)
                   | MemberLeaving
                   | MemberLeft (List SerfMember)
                   | MemberFailed
                   | StreamFailed
                   | UserEvent String LamportClock Boolean a

foreign import joinImpl :: (ApiError -> (SerfResult Int)) -> (Int -> SerfResult Int) -> IpAndPort -> List IpAndPort -> Boolean ->  Effect (SerfResult Int)
foreign import leaveImpl :: (ApiError -> (SerfResult Unit)) -> (Unit -> SerfResult Unit) -> IpAndPort ->  Effect (SerfResult Unit)
foreign import eventImpl :: forall a. (ApiError -> (SerfResult Unit)) -> (SerfResult Unit) -> IpAndPort -> String -> a -> Boolean ->  Effect (SerfResult Unit)
foreign import streamImpl :: (ApiError -> (SerfResult Unit)) -> (SerfResult Unit) -> IpAndPort -> Effect (SerfResult Unit)
foreign import membersImpl :: (ApiError -> (SerfResult (List SerfMember))) -> ((List SerfMember) -> SerfResult (List SerfMember)) -> IpAndPort -> Effect (SerfResult (List SerfMember))
foreign import getCoordinateImpl :: (ApiError -> (SerfResult SerfCoordinate)) -> (SerfCoordinate -> SerfResult SerfCoordinate) -> IpAndPort -> String -> Effect (SerfResult SerfCoordinate)
foreign import messageMapperImpl :: forall a. Foreign -> Maybe (SerfMessage a)

data Ip = Ipv4 Int Int Int Int

instance showAgent :: Show Ip where
  show (Ipv4 a b c d) = (show a) <> "." <> (show b) <> "." <> (show c) <> "." <> (show d)

data ApiError = SerfError String
              | NetworkError Foreign

type IpAndPort = { ip :: String
                 , port :: Int
                 }

strToIp :: String -> Maybe Ip
strToIp str =
  str
  # split (Pattern ".")
  # traverse fromString
  # (\ip -> case ip of
        Just [a, b, c, d] -> Just (Ipv4 a b c d)
        _ -> Nothing
    )

join :: IpAndPort -> List IpAndPort -> Boolean -> Effect (SerfResult Int)
join = joinImpl Left Right

leave :: IpAndPort -> Effect (SerfResult Unit)
leave = leaveImpl Left Right

event :: forall a. IpAndPort -> String -> a -> Boolean ->  Effect (SerfResult Unit)
event = eventImpl Left (Right unit)

stream :: IpAndPort -> Effect (SerfResult Unit)
stream = streamImpl Left (Right unit)

members :: IpAndPort -> Effect (SerfResult (List SerfMember))
members = membersImpl Left Right

getCoordinate :: IpAndPort -> String -> Effect (SerfResult SerfCoordinate)
getCoordinate = getCoordinateImpl Left Right

-- Calculation as described in https://www.serf.io/docs/internals/coordinates.html
calcRtt :: SerfCoordinate -> SerfCoordinate -> Milliseconds
calcRtt lhs rhs = wrap $
  let sumq = foldl (\acc (Tuple a b) -> acc + ((a - b) * (a - b))) 0.0 (zip lhs.vec rhs.vec)
      rtt = sqrt sumq + lhs.height + rhs.height
      adjusted = rtt + lhs.adjustment + rhs.adjustment
  in
  if adjusted > 0.0
    then adjusted * 1000.0
    else rtt * 1000.0


messageMapper :: forall a. Foreign -> Maybe (SerfMessage a)
messageMapper = messageMapperImpl
