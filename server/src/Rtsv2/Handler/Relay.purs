module Rtsv2.Handler.Relay
       ( resource
       , register
       , stats
       ) where


import PintoHelper (GenericHandlerState, GenericStatusState, genericPost, genericStatus)
import Rtsv2.Agents.StreamRelayInstance (CreateRelayPayload, RegisterEgestPayload)
import Rtsv2.Agents.StreamRelayInstance as StreamRelayInstance
import Rtsv2.Agents.StreamRelayInstanceSup as StreamRelayInstanceSup
import Shared.Types.Agent.State as PublicState
import Stetson (StetsonHandler)


stats :: StetsonHandler (GenericStatusState PublicState.StreamRelay)
stats = genericStatus StreamRelayInstance.status

resource :: StetsonHandler (GenericHandlerState CreateRelayPayload)
resource = genericPost  StreamRelayInstanceSup.startRelay

register :: StetsonHandler (GenericHandlerState RegisterEgestPayload)
register = genericPost  StreamRelayInstance.registerEgest
