module Component.HTML.Dropdown where

import Prelude

import Control.MonadPlus (guard)
import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import NSelect as Select
import Rtsv2App.Capability.Navigate (class Navigate)
import Rtsv2App.Component.HTML.Utils (css)

type Query = Const Void

data Action
  = OnInput String
  | HandleDropdown (Select.Message Action)

type State =
  { value :: String
  }

type Slots =
  ( dropdown :: Select.Slot Action Unit
  )

_dropdown = SProxy :: SProxy "dropdown"


component
  :: forall m
   . MonadAff m
  => Navigate m
  => H.Component HH.HTML (Const Void) Unit Void m
component = H.mkComponent
  { initialState: const initialState
  , render
  , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction }
  }
  where
  initialState :: State
  initialState =
    { value: ""
    }

  handleAction = case _ of
    OnInput value -> H.modify_ $ _ { value = value }

    HandleDropdown msg -> do
      case msg of
        Select.Emit q -> handleAction q
        _ -> pure unit

  render :: State -> H.ComponentHTML Action Slots m
  render state =
    HH.div_
    [ HH.p
      [ css "mb-3"]
      [ HH.text "Trigger parent action from dropdown."]
    , HH.slot _dropdown unit Select.component
      { render: renderSelect state
      , itemCount: 0
      } $ Just <<< HandleDropdown
    ]


renderSelect
  :: forall m
   . MonadAff m
  => Navigate m
  => State
  -> Select.State
  -> Select.HTML Action () m
renderSelect state st =
  HH.div
  ( Select.setRootProps [ css "inline-block"]
  ) $ join
  [ pure $ HH.button
    ( Select.setToggleProps [])
    [ HH.text "toggle" ]
  , guard st.isOpen $> HH.div
    [ css "Dropdown p-4"
    ]
    [ HH.input
      [ HP.value state.value
      , HE.onValueInput $ Just <<< Select.raise <<< OnInput
      ]
    , HH.div_
      [ HH.text $ "You typed: " <> state.value
      ]
    ]
  ]
