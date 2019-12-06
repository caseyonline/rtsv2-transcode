module Rtsv2.IntraPoPSerf
       ( IntraMessage(..)
       , module RootSerf
       , event
       , messageMapper
--       , originServer
       )

       where

import Prelude

import Data.Maybe (Maybe(..))
--import Debug.Trace (spy)
import Effect (Effect)
import Erl.Data.List (List)
import Foreign (Foreign)
import Rtsv2.PoPDefinition (ServerAddress)
import Serf (strToIp, join, leave, stream, Ip(..), IpAndPort, ApiError(..), SerfResult, SerfMember) as RootSerf
import Serf as Serf
import Shared.Stream (StreamId)

data IntraMessage = StreamAvailable StreamId ServerAddress
                  | TransPoPLeader ServerAddress
                  | MembersAlive (List Serf.SerfMember)
                  | MembersLeft (List Serf.SerfMember)
                  | Ignore

event :: Serf.IpAndPort -> String -> IntraMessage -> Boolean ->  Effect (Serf.SerfResult Unit)
event = Serf.event

-- originServer :: IntraMessage -> Maybe ServerAddress
-- originServer (StreamAvailable _ serverAddress) = Just serverAddress
-- originServer (TransPoPLeader serverAddress) = Just serverAddress
-- originServer (MembersAlive _) = Nothing
-- originServer (MembersLeft _) = Nothing
-- originServer Ignore = Nothing

messageMapper :: Foreign -> Maybe IntraMessage
messageMapper f =
  case Serf.messageMapper f of
    Nothing -> Nothing
    Just a ->
      case a of
        Serf.MemberAlive members -> Just (MembersAlive members)
        Serf.MemberLeaving -> Just Ignore
        Serf.MemberLeft members -> Just (MembersLeft members)
        Serf.MemberFailed -> Just Ignore
        Serf.StreamFailed -> Just Ignore
        Serf.UserEvent _ _ _ msg -> Just msg
