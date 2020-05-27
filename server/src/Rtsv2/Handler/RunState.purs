module Rtsv2.Handler.RunState
       (
         setRunState
       ) where

import Rtsv2.NodeManager as NodeManager
import Shared.Rtsv2.Types (RunState)
import StetsonHelper (PostHandler, processPostPayload)

setRunState :: PostHandler RunState
setRunState = processPostPayload NodeManager.changeRunState
