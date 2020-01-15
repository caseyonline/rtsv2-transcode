module Rtsv2App.Page.Home where

import Prelude

import Component.HOC.Connect as Connect
import Control.Monad.Reader (class MonadAsk)
import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Rtsv2App.Capability.Navigate (class Navigate)
import Rtsv2App.Component.HTML.Footer (footer)
import Rtsv2App.Component.HTML.Header as HD
import Rtsv2App.Component.HTML.MainMenu as MM
import Rtsv2App.Component.HTML.Utils (css, dataAttr)
import Rtsv2App.Data.Profile (Profile)
import Rtsv2App.Data.Route (Route(..))
import Rtsv2App.Env (UserEnv)

data Action
  = Initialize
  | Receive { currentUser :: Maybe Profile }

type State =
  { page :: Int
  , currentUser :: Maybe Profile
  }

type ChildSlots =
  ( mainMenu :: MM.Slot Unit
  , header :: MM.Slot Unit
  )

component
  :: forall m r
   . MonadAff m
  => MonadAsk { userEnv :: UserEnv | r } m
  => Navigate m
  => H.Component HH.HTML (Const Void) {} Void m
component = Connect.component $ H.mkComponent
  { initialState
  , render
  , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      , receive = Just <<< Receive
      , initialize = Just Initialize
      }
  }
  where
  initialState { currentUser } =
    { currentUser
    , page: 1
    }
    
  handleAction :: Action -> H.HalogenM State Action ChildSlots Void m Unit
  handleAction = case _ of
    Initialize -> do
      state <- H.get
      case state.currentUser of
        Nothing -> pure unit
        profile -> pure unit

    Receive { currentUser } ->
      H.modify_ _ { currentUser = currentUser }

  render :: State -> H.ComponentHTML Action ChildSlots m
  render state@{ currentUser } =
    HH.div
      [ css "main" ]
      [ HH.slot (SProxy :: _ "header") unit HD.component { currentUser, route: Login } absurd
      , HH.slot (SProxy :: _ "mainMenu") unit MM.component { currentUser, route: Home } absurd
      , HH.div
        [ css "app-content content" ]
        [ HH.div
          [ css "content-wrapper" ]
          [ HH.div
            [ css "content-wrapper-before" ]
            []
          , HH.div
            [ css "content-header row" ]
            [ HH.div
              [ css "content-header-left col-md-4 col-12 mb-2" ]
              [ HH.h3
                [ css "content-header-h3" ]
                [ HH.text "Home" ]
              ]
            ]
          , HH.div
            [ css "content-body" ]
            [ HH.div
              [ css "row" ]
              [ HH.div
                [ css "col-12" ]
                [ HH.div
                  [ css "card" ]
                  html
                ]
              ]
            ]
          ]
        ]
      , footer
      ]
    where
      html =
        [ HH.div
          [ css "card-body" ]
          [ HH.img
            [ css "col-12"
            , HP.src "assets/images/backgrounds/world-map.png"
            ]
          ]
        ]
