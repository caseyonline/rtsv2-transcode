module Rtsv2.Handler.Egest
       ( resource
       , relayResource
       ) where


import PintoHelper (GenericHandlerState, genericPost)
import Rtsv2.Agents.EgestInstance (CreateEgestPayload)
import Rtsv2.Agents.EgestInstanceSup as EgestInstanceSup
import Stetson (StetsonHandler)


resource :: StetsonHandler (GenericHandlerState CreateEgestPayload)
resource = genericPost EgestInstanceSup.startEgest

relayResource :: StetsonHandler (GenericHandlerState CreateEgestPayload)
relayResource = genericPost EgestInstanceSup.startEgest
