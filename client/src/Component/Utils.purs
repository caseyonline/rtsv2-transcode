module Rtsv2App.Component.Utils
       ( Notification(..)
       , NotificationContent
       , NotificationMessage(..)
       , OpaqueSlot
       , PoPAggrSelectedInfo
       , SlotDetailsArgs
       , busEventSource
       , unwrapNotification
       )
       where

import Prelude

import Control.Monad.Rec.Class (forever)
import Data.Const (Const)
import Effect.Aff (error, forkAff, killFiber)
import Effect.Aff.Bus as Bus
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.Query.EventSource as ES
import Shared.Rtsv2.Stream (SlotId, SlotRole)
import Shared.Rtsv2.Types (PoPName, ServerAddress)
import Data.Maybe (Maybe)

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------
type OpaqueSlot = H.Slot (Const Void) NotificationMessage

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
    DangerN n
  | DarkN n
  | InfoN n
  | LightN n
  | PrimaryN n
  | SuccessN n
  | WarningN n


-------------------------------------------------------------------------------
-- Functions
-------------------------------------------------------------------------------
unwrapNotification :: forall a. Notification a -> a
unwrapNotification notification =
  case notification of
    DangerN  n -> n
    DarkN    n -> n
    InfoN    n -> n
    LightN   n -> n
    PrimaryN n -> n
    SuccessN n -> n
    WarningN n -> n

busEventSource :: forall m r act. MonadAff m => Bus.BusR' r act -> ES.EventSource m act
busEventSource bus =
  ES.affEventSource \emitter -> do
    fiber <- forkAff $ forever $ ES.emit emitter =<< Bus.read bus
    pure (ES.Finalizer (killFiber (error "Event source closed") fiber))
