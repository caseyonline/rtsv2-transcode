module Rtsv2.TransPoPSerf
       -- ( module RootSerf
       -- , event
       -- , messageMapper
       -- , origin
       -- )

       where

foo = 1
-- import Prelude

-- import Data.Maybe (Maybe(..))
-- import Effect (Effect)
-- import Erl.Data.List (List)
-- import Foreign (Foreign)
-- import Rtsv2.PoPDefinition (ServerAddress)
-- import Serf (strToIp, join, leave, stream, Ip(..), IpAndPort, ApiError(..), SerfResult, SerfMember) as RootSerf
-- import Serf as Serf
-- import Shared.Stream (StreamId)


-- event :: RootSerf.IpAndPort -> String -> TransMessage -> Boolean ->  Effect (RootSerf.SerfResult Unit)
-- event = Serf.event


-- messageMapper :: Foreign -> Maybe TransMessage
-- messageMapper f =
--   case Serf.messageMapper f of
--     Nothing -> Nothing
--     Just a ->
--       case a of
--         Serf.MemberAlive _ -> Just Ignore
--         Serf.MemberLeaving -> Just Ignore
--         Serf.MemberLeft _ -> Just Ignore
--         Serf.MemberFailed -> Just Ignore
--         Serf.StreamFailed -> Just SerfStreamFailed
--         Serf.UserEvent _ _ _ msg -> Just msg
