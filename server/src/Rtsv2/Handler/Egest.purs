module Rtsv2.Handler.Egest
       ( resource
       , relayResource
       ) where


import Rtsv2.Agents.EgestInstance (CreateEgestPayload)
import Rtsv2.Agents.EgestInstanceSup as EgestInstanceSup
import StetsonHelper (GenericStetsonHandler, genericPost)

resource :: GenericStetsonHandler CreateEgestPayload
resource = genericPost EgestInstanceSup.startEgest

relayResource :: GenericStetsonHandler CreateEgestPayload
relayResource = genericPost EgestInstanceSup.startEgest
