module Rtsv2.TransPoPSerf
       ( TransMessage(..)
       , module RootSerf
       , event
       , messageMapper
       , origin
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

data TransMessage = StreamAvailable StreamId ServerAddress
                  | Ignore

event :: RootSerf.IpAndPort -> String -> TransMessage -> Boolean ->  Effect (RootSerf.SerfResult Unit)
event = Serf.event

origin :: TransMessage -> Maybe ServerAddress
origin (StreamAvailable _ serverAddress) = Just serverAddress
origin Ignore = Nothing

messageMapper :: Foreign -> Maybe TransMessage
messageMapper f =
  case Serf.messageMapper f of
    Nothing -> Nothing
    Just a ->
      case a of
        Serf.MemberAlive _ -> Just Ignore
        Serf.MemberLeaving -> Just Ignore
        Serf.MemberLeft _ -> Just Ignore
        Serf.MemberFailed -> Just Ignore
        Serf.StreamFailed -> Just Ignore
        Serf.UserEvent _ _ _ msg -> Just msg
