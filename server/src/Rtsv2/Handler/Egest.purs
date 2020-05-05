module Rtsv2.Handler.Egest
       ( startResource
       ) where


import Rtsv2.Agents.EgestInstance (CreateEgestPayload)
import Rtsv2.Agents.EgestInstanceSup as EgestInstanceSup
import Rtsv2.Config (LoadConfig)
import StetsonHelper (PostHandler, processPostPayload)

startResource :: LoadConfig -> PostHandler CreateEgestPayload
startResource loadConfig = processPostPayload (EgestInstanceSup.startLocalEgest loadConfig)
