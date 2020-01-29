module Rtsv2.Handler.Relay
       ( resource
       , register
       , stats
       ) where

import Rtsv2.Agents.StreamRelayInstance (CreateRelayPayload, RegisterEgestPayload)
import Rtsv2.Agents.StreamRelayInstance as StreamRelayInstance
import Rtsv2.Agents.StreamRelayInstanceSup as StreamRelayInstanceSup
import Shared.Types.Agent.State as PublicState
import StetsonHelper (GenericStetsonGetByStreamId, GenericStetsonHandler, genericGetByStreamId, genericPost)


stats :: GenericStetsonGetByStreamId PublicState.StreamRelay
stats = genericGetByStreamId StreamRelayInstance.status

resource :: GenericStetsonHandler CreateRelayPayload
resource =  genericPost  StreamRelayInstanceSup.startRelay

register :: GenericStetsonHandler RegisterEgestPayload
register = genericPost  StreamRelayInstance.registerEgest
