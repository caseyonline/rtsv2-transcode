module Rtsv2App.Component.HTML.NotificationMain where


import Prelude

import Data.Array (deleteAt, mapWithIndex)
import Data.Const (Const)
import Data.Function (flip)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Debug.Trace (traceM)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Rtsv2App.Component.Utils (Notification, NotificationContent, NotificationMessage(..))
import Rtsv2App.Component.HTML.NotificationDialog (Message(..))
import Rtsv2App.Component.HTML.NotificationDialog as ND
import Rtsv2App.Component.HTML.Utils (css_)

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------
type Input = { notifications :: (Array (Notification NotificationContent)) }

type Slot = H.Slot (Const Void) 

data Action
  = CloseDialog ND.Message
  | Receive Input

type State =
  { notifications :: (Array (Notification NotificationContent)) }

type ChildSlots =
  ( notificationDialog :: ND.Slot Int
  )

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

  handleAction :: Action -> H.HalogenM State Action ChildSlots NotificationMessage m Unit
  handleAction = case _ of
    Receive { notifications } -> H.put { notifications: notifications }

    CloseDialog (NDialogMsg index) -> do
      st <- H.get
      case deleteAt index st.notifications of
        Nothing -> pure unit
        Just n  -> do
          H.raise (NMessages n)
          H.modify_ _ { notifications = n }

  render :: State -> H.ComponentHTML Action ChildSlots m
  render state@{ notifications } =
    HH.div
    [ css_ $ "notices is-bottom"  ]
    (flip mapWithIndex notifications
      (\index notification ->
         HH.slot (SProxy :: _ "notificationDialog") index ND.component { notification, index } (Just <<< CloseDialog)
      )
     )
