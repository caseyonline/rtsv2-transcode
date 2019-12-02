module Rtsv2.Serf
       -- ( strToIp
       -- , join
       -- , event
       -- , stream
       -- , messageMapper
       -- , Ip(..), IpAndPort(..), ApiError(..), Message(..)
       -- )
       ( StateMessage(..)
       , module RootSerf
       , event
       , messageMapper
       )

       where



import Prelude

import Effect (Effect)
import Foreign (Foreign)
import Rtsv2.PoPDefinition (ServerAddress)
import Serf (strToIp, join, stream, Ip(..), IpAndPort, ApiError(..), Message(..), SerfResult) as RootSerf
import Serf as Serf
import Shared.Stream (StreamId)


data StateMessage = StreamAvailable StreamId ServerAddress


event :: RootSerf.IpAndPort -> String -> StateMessage -> Boolean ->  Effect (RootSerf.SerfResult Unit)
event = Serf.event


messageMapper :: Foreign -> Serf.Message StateMessage
messageMapper = Serf.messageMapper
