module Rtsv2.Handler.Canary
       (
         setCanary
       ) where

import Rtsv2.NodeManager as NodeManager
import Shared.Rtsv2.Types (CanaryState)
import StetsonHelper (PostHandler, processPostPayload)

setCanary :: PostHandler CanaryState
setCanary = processPostPayload NodeManager.changeCanaryState
