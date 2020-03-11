module Rtsv2App.Component.HTML.NotificationDialog where


import Prelude

import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Rtsv2App.Component.HTML.Utils (css_)


data Action
  = CloseDialog
  | Receive Input


type State =
  { message  :: String
  , cssStyle :: String
  }

type Input =
  { message  :: String
  , cssStyle :: String
  }

type Slot
  = H.Slot (Const Void) Void

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
  initialState { message, cssStyle } =
    { message
    , cssStyle
    }

  handleAction = case _ of
    Receive s -> do
       H.put s

    CloseDialog -> pure unit

  render state@{ message, cssStyle } =
    HH.div
    [ css_ $ "snackbar is-bottom-right " <> ""  ]
    [ HH.div
      [ css_ "text" ]
      [ HH.text message ]
    , HH.div
      [ css_ $ "acction " <> "" ]
      [ HH.button
        [ css_ "button"
        , HE.onClick $ const $ Just CloseDialog
        ]
        [ HH.text "OK" ]
      ]
    ]
