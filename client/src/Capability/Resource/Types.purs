module Rtsv2App.Capability.Resource.Types
       ( SlotDetailsArgs
       , PoPAggrSelectedInfo
       , Notification
       , NotificationMessage
       )
       where

import Data.Maybe (Maybe)
import Shared.Stream (SlotId, SlotRole)
import Shared.Types (PoPName, ServerAddress)


type SlotDetailsArgs =
  { slotId        :: SlotId
  , slotRole      :: SlotRole
  , serverAddress :: ServerAddress
  }


type PoPAggrSelectedInfo =
    { selectedPoPName   :: Maybe PoPName
    , selectedSlotId    :: Maybe SlotId
    , selectedAddress   :: Maybe ServerAddress
    , selectedAggrIndex :: Maybe Int
    }

type Notification =
  { message  :: String
  , cssStyle :: String
  }

data NotificationMessage = NotificationMessage (Array Notification) 
