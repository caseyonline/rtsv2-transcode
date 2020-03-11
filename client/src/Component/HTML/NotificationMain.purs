module Rtsv2App.Component.HTML.Notification.NotificationMain where


import Prelude

import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Rtsv2App.Capability.Resource.Types (Notification)
import Rtsv2App.Component.HTML.Utils (css_)

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------
type Input = Array Notification

type Slot = H.Slot (Const Void) Message

data Message = NotificationMsg (Array Notification)

data Action
  = CloseDialog
  | Receive Input

type State =
  { notifications :: (Array Notification) }


component
  :: forall m
   . MonadAff m
  => H.Component HH.HTML (Const Void) Input Void m
component = H.mkComponent
  { initialState: initialState
  , render
  , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      , receive = Just <<< Receive
      }
  }
  where
  initialState :: Input -> State
  initialState notifications =
    { notifications }

  handleAction = case _ of
    Receive s -> do
       H.put { notifications: s }

    CloseDialog -> pure unit

  render state@{ notifications } =
    HH.div
    [ css_ $ "notice is-bottom"  ]
    [ 
    ]
