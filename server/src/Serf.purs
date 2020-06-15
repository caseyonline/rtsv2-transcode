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
       , class SerfWireMessage, toWireMessage, fromWireMessage
       )
       where

import Prelude

import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Int (fromString, round)
import Data.Long as Long
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), split)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Erl.Atom (Atom)
import Erl.Data.Binary (Binary)
import Erl.Data.List (List, zip)
import Erl.Data.Map (Map)
import Erl.Data.Tuple (Tuple2, fst, snd)
import Foreign (Foreign)
import Math (sqrt)
import Shared.Common (Milliseconds(..))
import Unsafe.Coerce (unsafeCoerce)

type SerfResult a = Either ApiError a

class SerfWireMessage a where
  toWireMessage :: a -> Tuple2 String Binary
  fromWireMessage :: String -> Binary -> Maybe a


class SerfWireElement a where
  toWireElement :: a -> Binary
  fromWireElement :: Binary -> a

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
foreign import eventImpl :: (ApiError -> (SerfResult Unit)) -> (SerfResult Unit) -> IpAndPort -> String -> Binary -> Boolean ->  Effect (SerfResult Unit)
foreign import streamImpl :: (ApiError -> (SerfResult Unit)) -> (SerfResult Unit) -> IpAndPort -> Effect (SerfResult Unit)
foreign import membersImpl :: (ApiError -> (SerfResult (List SerfMember))) -> ((List SerfMember) -> SerfResult (List SerfMember)) -> IpAndPort -> Effect (SerfResult (List SerfMember))
foreign import getCoordinateImpl :: (ApiError -> (SerfResult SerfCoordinate)) -> (SerfCoordinate -> SerfResult SerfCoordinate) -> IpAndPort -> String -> Effect (SerfResult SerfCoordinate)
foreign import messageMapperImpl :: Foreign -> Maybe (SerfMessage Binary)

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

event :: forall a. SerfWireMessage a => IpAndPort -> a -> Boolean ->  Effect (SerfResult Unit)
event rpcAddr msg =
  let wireMsg = toWireMessage msg
  in
    eventImpl Left (Right unit) rpcAddr (fst wireMsg) (snd wireMsg)

stream :: IpAndPort -> Effect (SerfResult Unit)
stream = streamImpl Left (Right unit)

members :: IpAndPort -> Effect (SerfResult (List SerfMember))
members = membersImpl Left Right

getCoordinate :: IpAndPort -> String -> Effect (SerfResult SerfCoordinate)
getCoordinate = getCoordinateImpl Left Right

-- Calculation as described in https://www.serf.io/docs/internals/coordinates.html
calcRtt :: SerfCoordinate -> SerfCoordinate -> Milliseconds
calcRtt lhs rhs = Milliseconds $ Long.fromInt $ round $
  let sumq = foldl (\acc (Tuple a b) -> acc + ((a - b) * (a - b))) 0.0 (zip lhs.vec rhs.vec)
      rtt = sqrt sumq + lhs.height + rhs.height
      adjusted = rtt + lhs.adjustment + rhs.adjustment
  in
  if adjusted > 0.0
    then adjusted * 1000.0
    else rtt * 1000.0


messageMapper :: forall a. SerfWireMessage a => Foreign -> Maybe (SerfMessage a)
messageMapper = do
  mapUserPayload <=< messageMapperImpl
  where
    mapUserPayload :: SerfWireMessage a => SerfMessage Binary -> Maybe (SerfMessage a)
    mapUserPayload (UserEvent name lamportClock coalesce payload) =
      case fromWireMessage name payload of
        Nothing -> Nothing
        Just mappedPayload -> Just $ UserEvent name lamportClock coalesce mappedPayload
    mapUserPayload other = Just $ unsafeCoerce other
