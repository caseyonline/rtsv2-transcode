module Rtsv2App.Component.HTML.Header where

import Prelude

import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Rtsv2App.Capability.Navigate (class Navigate, logout)
import Rtsv2App.Component.HTML.Utils (css_, dataAttr, maybeElem, safeHref)
import Rtsv2App.Data.Profile (Profile)
import Rtsv2App.Data.Route (Route(..))


-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------
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


-------------------------------------------------------------------------------
-- Header Component
-------------------------------------------------------------------------------
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
        [ css_ "header-navbar navbar-expand-md navbar navbar-with-menu navbar-without-dd-arrow fixed-top navbar-semi-light" ]
        [ HH.div
        [ css_ "navbar-wrapper" ]
        [ HH.div
            [ css_ "navbar-container content" ]
            [ HH.div
            [ css_ "collapse navbar-collapse show"
            , HP.id_ "navbar-mobile"]
            [ HH.ul
                [ css_ "nav navbar-nav mr-auto float-left" ]
                [ HH.li
                [ css_ "nav-item d-block d-md-none" ]
                [ HH.a
                    [ css_ "nav-link nav-menu-main menu-toggle hidden-xs" ]
                    [ HH.i
                    [ css_ "ft-menu" ]
                    []
                    ]
                ]
                ]
            , HH.ul
                [ css_ "nav navbar-nav float-right" ]
                [ HH.li
                [ css_ "dropdown dropdown-user nav-item" ]
                [ HH.a
                    [ css_ "dropdown-toggle nav-link dropdown-user-link"
                    , dataAttr "toggle" "dropdown"
                    ]
                    [ HH.span
                    [ css_ "avatar" ]
                    [ HH.img
                        [  HP.src "assets/images/avatar-s.png" ]
                    , HH.i
                        [ css_ "" ]
                        []
                    ]
                    ]
                , maybeElem currentUser \cu ->
                    HH.div
                    [ css_ "dropdown-menu dropdown-menu-right" ]
                    [ HH.div
                        [ css_ "arrow_box_right" ]
                        [ HH.a
                        [ css_ "dropdown-item"
                        , safeHref $ Settings
                        ]
                        [ HH.i
                            [ css_ "ft-user" ]
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
