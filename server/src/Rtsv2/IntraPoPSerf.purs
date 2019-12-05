module Rtsv2.IntraPoPSerf
       ( IntraMessage(..)
       , module RootSerf
       , event
       , messageMapper
       , originServer
       )

       where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Foreign (Foreign)
import Rtsv2.PoPDefinition (ServerAddress)
import Serf (strToIp, join, leave, stream, Ip(..), IpAndPort, ApiError(..), SerfResult) as RootSerf
import Serf as Serf
import Shared.Stream (StreamId)

data IntraMessage = StreamAvailable StreamId ServerAddress
                  | TransPoPLeader ServerAddress
                  | Ignore

event :: RootSerf.IpAndPort -> String -> IntraMessage -> Boolean ->  Effect (RootSerf.SerfResult Unit)
event = Serf.event

originServer :: IntraMessage -> Maybe ServerAddress
originServer (StreamAvailable _ serverAddress) = Just serverAddress
originServer (TransPoPLeader serverAddress) = Just serverAddress
originServer Ignore = Nothing

messageMapper :: Foreign -> Maybe IntraMessage
messageMapper f =
  case Serf.messageMapper f of
    Nothing -> Nothing
    Just a ->
      case a of
        Serf.MemberAlive -> Just Ignore
        Serf.MemberLeaving -> Just Ignore
        Serf.MemberLeft -> Just Ignore
        Serf.MemberFailed -> Just Ignore
        Serf.StreamFailed -> Just Ignore
        Serf.UserEvent _ _ _ msg -> Just msg
