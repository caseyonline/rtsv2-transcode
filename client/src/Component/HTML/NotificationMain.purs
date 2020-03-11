module Rtsv2App.Component.HTML.NotificationMain where


import Prelude

import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Rtsv2App.Capability.Resource.Types (Notification, NotificationMessage)
import Rtsv2App.Component.HTML.Utils (css_)

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------
type Input = { notifications :: Array Notification }

type Slot = H.Slot (Const Void) NotificationMessage

data Action
  = CloseDialog
  | Receive Input

type State =
  { notifications :: (Array Notification) }


component
  :: forall m
   . MonadAff m
  => H.Component HH.HTML (Const Void) Input NotificationMessage m
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
  initialState { notifications } =
    { notifications }

  handleAction = case _ of
    Receive { notifications } -> do
       H.put { notifications: notifications }

    CloseDialog -> pure unit

  render state@{ notifications } =
    HH.div
    [ css_ $ "notice is-bottom"  ]
    [ 
    ]
