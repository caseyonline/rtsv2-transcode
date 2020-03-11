module Rtsv2App.Component.HTML.NotificationDialog where


import Prelude

import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Rtsv2App.Capability.Resource.Types (Notification(..), NotificationMessage, NotificationContent)
import Rtsv2App.Component.HTML.Utils (css_)


data Action
  = CloseDialog Int
  | Receive Input

data Message = NDialogMsg Int

type State = NDialogContent

type NDialogContent =
  { message  :: String
  , cssStyle :: String
  , index    :: Int
  }

type Input =
  { notification :: Notification NotificationContent
  , index :: Int
  }
  
type Slot = H.Slot (Const Void) Message

component
  :: forall m
   . MonadAff m
  => H.Component HH.HTML (Const Void) Input Message m
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
  initialState notification = toNDialogContent notification

  handleAction :: Action -> H.HalogenM State Action () Message m Unit
  handleAction = case _ of
    Receive notification -> H.put $ toNDialogContent notification      

    CloseDialog index -> H.raise $ NDialogMsg index

  render state@{ message, cssStyle, index } =
    HH.div
    [ css_ $ "snackbar is-bottom-right fadeIn " <> cssStyle ]
    [ HH.div
      [ css_ "text" ]
      [ HH.text message ]
    , HH.div
      [ css_ $ "action " <> cssStyle ]
      [ HH.button
        [ css_ "button"
        , HE.onClick $ const $ Just $ CloseDialog index
        ]
        [ HH.text "OK" ]
      ]
    ]

toNDialogContent :: Input -> State
toNDialogContent { notification, index } =
  case notification of
    Danger  n -> { message: n.message , cssStyle: "is-danger", index }
    Dark    n -> { message: n.message , cssStyle: "is-dark", index }
    Info    n -> { message: n.message , cssStyle: "is-info", index }
    Light   n -> { message: n.message , cssStyle: "is-light", index }
    Primary n -> { message: n.message , cssStyle: "is-primary", index }
    Success n -> { message: n.message , cssStyle: "is-success", index }
    Warning n -> { message: n.message , cssStyle: "is-warning", index }

-- <div class="notices is-bottom">
--   <div role="alertdialog" class="snackbar is-success is-bottom-right fadeOut v-leave-to" style="">
--     <div class="text">Got click</div> 
--     <div class="action is-success">
--       <button class="button">OK</button>
--     </div>
--   </div>
-- </div>
