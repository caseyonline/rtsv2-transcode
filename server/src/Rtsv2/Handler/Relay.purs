module Rtsv2.Handler.Relay
       ( resource
       , stats
       ) where


import PintoHelper (GenericHandlerState, GenericStatusState, genericCreate, genericStatus)
import Rtsv2.Agents.StreamRelayInstance (CreateRelayPayload)
import Rtsv2.Agents.StreamRelayInstance as StreamRelayInstance
import Rtsv2.Agents.StreamRelayInstanceSup as StreamRelayInstanceSup
import Shared.Types.Agent.State as PublicState
import Stetson (StetsonHandler)


stats :: StetsonHandler (GenericStatusState PublicState.StreamRelay)
stats = genericStatus StreamRelayInstance.status

resource :: StetsonHandler (GenericHandlerState CreateRelayPayload)
resource = genericCreate  StreamRelayInstanceSup.startRelay
