module Rtsv2.Handler.Canary
       (
         setCanary
       ) where

import Prelude

import Rtsv2.NodeManager as NodeManager
import Shared.Rtsv2.Types (Canary)
import StetsonHelper (PostHandler, processPostPayload)

setCanary :: PostHandler Canary
setCanary = processPostPayload NodeManager.changeCanaryState
