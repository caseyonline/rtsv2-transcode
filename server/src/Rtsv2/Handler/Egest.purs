module Rtsv2.Handler.Egest
       ( resource
       , relayResource
       ) where


import Rtsv2.Agents.EgestInstance (CreateEgestPayload)
import Rtsv2.Agents.EgestInstanceSup as EgestInstanceSup
import StetsonHelper (PostHandler, processPostPayload)

resource :: PostHandler CreateEgestPayload
resource = processPostPayload EgestInstanceSup.startEgest

relayResource :: PostHandler CreateEgestPayload
relayResource = processPostPayload EgestInstanceSup.startEgest
