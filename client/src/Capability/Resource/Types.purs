module Rtsv2App.Capability.Resource.Types
       ( SlotDetailsArgs
       , PoPAggrSelectedInfo
       , Notification(..)
       , NotificationContent
       , NotificationMessage(..)
       , unwrapNotification
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

type NotificationContent =
  { message   :: String
  , title     :: String
  , autoClose :: Boolean
  }

data NotificationMessage =
    NMessages (Array (Notification NotificationContent))
  | NSingleMessage (Notification NotificationContent)

data Notification n =
    Danger n
  | Dark n
  | Info n
  | Light n
  | Primary n
  | Success n
  | Warning n


unwrapNotification :: forall a. Notification a -> a
unwrapNotification notification =
  case notification of
    Danger  n -> n
    Dark    n -> n
    Info    n -> n
    Light   n -> n
    Primary n -> n
    Success n -> n
    Warning n -> n
