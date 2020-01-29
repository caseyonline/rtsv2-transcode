module Rtsv2App.Component.HTML.Header where

import Prelude

import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Rtsv2App.Capability.Navigate (class Navigate, logout)
import Rtsv2App.Component.HTML.Utils (css, dataAttr, maybeElem, safeHref)
import Rtsv2App.Data.Profile (Profile)
import Rtsv2App.Data.Route (Route(..))


data Action
  = LogUserOut
  | Receive Input

type State =
  { currentUser :: Maybe Profile
  , route :: Route
  }

type Input =
  { currentUser :: Maybe Profile
  , route :: Route
  }

type Slot
  = H.Slot (Const Void) Void

component
  :: forall m
   . MonadAff m
  => Navigate m
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
  initialState { currentUser, route } =
    { currentUser
    , route
    }

  handleAction = case _ of
    Receive s -> do
       H.put s

    LogUserOut -> logout

  render state@{ currentUser, route } =
    HH.nav
        [ css "header-navbar navbar-expand-md navbar navbar-with-menu navbar-without-dd-arrow fixed-top navbar-semi-light" ]
        [ HH.div
        [ css "navbar-wrapper" ]
        [ HH.div
            [ css "navbar-container content" ]
            [ HH.div
            [ css "collapse navbar-collapse show"
            , HP.id_ "navbar-mobile"]
            [ HH.ul
                [ css "nav navbar-nav mr-auto float-left" ]
                [ HH.li
                [ css "nav-item d-block d-md-none" ]
                [ HH.a
                    [ css "nav-link nav-menu-main menu-toggle hidden-xs" ]
                    [ HH.i
                    [ css "ft-menu" ]
                    []
                    ]
                ]
                ]
            , HH.ul
                [ css "nav navbar-nav float-right" ]
                [ HH.li
                [ css "dropdown dropdown-user nav-item" ]
                [ HH.a
                    [ css "dropdown-toggle nav-link dropdown-user-link"
                    , dataAttr "toggle" "dropdown"
                    ]
                    [ HH.span
                    [ css "avatar" ]
                    [ HH.img
                        [  HP.src "assets/images/avatar-s.png" ]
                    , HH.i
                        [ css "" ]
                        []
                    ]
                    ]
                , maybeElem currentUser \cu ->
                    HH.div
                    [ css "dropdown-menu dropdown-menu-right" ]
                    [ HH.div
                        [ css "arrow_box_right" ]
                        [ HH.a
                        [ css "dropdown-item"
                        , safeHref $ Settings
                        ]
                        [ HH.i
                            [ css "ft-user" ]
                            []
                        , HH.text "Settings"
                        ]
                        ]
                    ]
                ]
                ]
            ]
            ]
        ]
        ]
