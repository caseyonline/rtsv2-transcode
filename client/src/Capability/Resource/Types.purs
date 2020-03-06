module Rtsv2App.Capability.Resource.Types
       ( SlotDetailsArgs
       , SelectedInfo
       )
       where

import Data.Maybe (Maybe)
import Shared.Stream (SlotId, SlotRole)
import Shared.Types (PoPName, ServerAddress, CheckBoxState)


type SlotDetailsArgs =
  { slotId        :: SlotId
  , slotRole      :: SlotRole
  , serverAddress :: ServerAddress
  }


type SelectedInfo =
    { selectedPoPName :: Maybe PoPName
    , selectedSlotId  :: Maybe SlotId
    , selectedAddress :: Maybe ServerAddress
    , checkedBoxes    :: Maybe (Array CheckBoxState)
    }
