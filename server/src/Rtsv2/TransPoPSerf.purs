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
import Erl.Data.List (List)
import Foreign (Foreign)
import Rtsv2.PoPDefinition (ServerAddress)
import Serf (strToIp, join, leave, stream, Ip(..), IpAndPort, ApiError(..), SerfResult, SerfMember) as RootSerf
import Serf as Serf
import Shared.Stream (StreamId)

data TransMessage = StreamAvailable StreamId ServerAddress
                  | MembersAlive (List RootSerf.SerfMember)
                  | MembersLeft (List RootSerf.SerfMember)
                  | SerfStreamFailed
                  | Ignore

event :: RootSerf.IpAndPort -> String -> TransMessage -> Boolean ->  Effect (RootSerf.SerfResult Unit)
event = Serf.event

origin :: TransMessage -> Maybe ServerAddress
origin (StreamAvailable _ serverAddress) = Just serverAddress
origin SerfStreamFailed = Nothing
origin (MembersAlive _) = Nothing
origin (MembersLeft _) = Nothing
origin Ignore = Nothing

messageMapper :: Foreign -> Maybe TransMessage
messageMapper f =
  case Serf.messageMapper f of
    Nothing -> Nothing
    Just a ->
      case a of
        Serf.MemberAlive members -> Just (MembersAlive members)
        Serf.MemberLeaving -> Just Ignore
        Serf.MemberLeft members -> Just (MembersLeft members)
        Serf.MemberFailed -> Just Ignore
        Serf.StreamFailed -> Just SerfStreamFailed
        Serf.UserEvent _ _ _ msg -> Just msg
