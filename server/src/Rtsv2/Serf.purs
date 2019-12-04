module Rtsv2.Serf
       ( StateMessage(..)
       , module RootSerf
       , event
       , messageMapper
       )

       where



import Prelude

import Data.Maybe (Maybe)
import Effect (Effect)
import Foreign (Foreign)
import Rtsv2.PoPDefinition (ServerAddress)
import Serf (strToIp, join, leave, stream, Ip(..), IpAndPort, ApiError(..), Message(..), SerfResult) as RootSerf
import Serf as Serf
import Shared.Stream (StreamId)

data StateMessage = StreamAvailable StreamId ServerAddress
                  | TransPoPLeader ServerAddress


event :: RootSerf.IpAndPort -> String -> StateMessage -> Boolean ->  Effect (RootSerf.SerfResult Unit)
event = Serf.event

messageMapper :: Foreign -> Maybe (Serf.Message StateMessage)
messageMapper = Serf.messageMapper
