module Rtsv2App.Component.HTML.MenuMain where

import Prelude

import Data.Const (Const)
import Data.Maybe (Maybe(..), isJust)
import Data.Monoid (guard)
import Data.Newtype (unwrap)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Elements as HL
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Rtsv2App.Capability.Navigate (class Navigate, logout)
import Rtsv2App.Component.HTML.Utils (css_, dataAttr, safeHref, whenElem)
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
    HH.aside
    [ css_ "aside is-placed-left is-expanded"
    , HP.id_ "aside-main"  
    ]
    [ topLogo
    , menuItems route
    , HH.div
      [ css_ "menu is-menu-bottom" ]
      [ HH.ul
        [ css_ "menu-list"]
        [ HH.li_
          [ HH.a
            [ css_ "has-icon is-state-info is-hoverable"
            , HE.onClick \_ -> Just LogUserOut
            , HP.title "Log Out"
            ]
            [ HH.span
              [ css_ "icon" ]
              [ HH.i
                [ css_ "mdi mdi-logout default" ]
                []
              ]
            , HH.span
              [ css_ "menu-item-label" ]
              [ HH.text "Log out" ]
            ]
          ]
        ]
      ]
    ]

-- | Logo Top title
topLogo :: forall p i. HH.HTML p i
topLogo =
  HH.div
  [ css_ "aside-tools" ]
  [ HH.div
    [ css_ "aside-tools-label" ]
    [ HH.span_
      [ HH.a
        [ safeHref Dashboard ]
        [ HH.span
          [ css_ "icon is-medium is-aligned-center" ]
          [ HH.img
            [ css_ "menu-logo"
            , HP.src "assets/images/logo/logo.svg"
            ]
          ]
        ]
      ]
    ]
  ]

-- | Menu items
menuItems :: forall p i. Route -> HH.HTML p i
menuItems route =
  HH.div
  [ css_ "menu-container jb-has-perfect-scrollbar" ]
  [ HH.div
    [ css_ "menu is-menu-main" ]
    [ HH.p
      [ css_ "menu-label" ]
      [ HH.text "General" ]
    ]
  , HH.ul
    [ css_ "menu-list" ]
    [ HH.li_
      [ HH.a
        [ css_ ("has-icon" <> guard (route == Dashboard) " is-active")
        , safeHref Dashboard
        ]
        [ HH.span
          [ css_ "icon" ]
          [ HH.i
            [ css_ "mdi menu mdi-desktop-mac" ]
            []
          ]
        , HH.span
          [ css_ "menu-item-label" ]
          [ HH.text "Dashboard" ]
        ]
      ]
    , HH.li_
      [ HH.a
        [ css_ "has-icon has-submenu-icon jb-aside-secondary-toggle" ]
        [ HH.span
          [ css_ "icon" ]
          [ HH.i
            [ css_ "mdi mdi-server-network" ]
            []
          ]
        , HH.span
          [ css_ "menu-item-label" ]
          [ HH.text "PoP" ]
        , HH.div
          [ css_ "submenu-icon" ]
          [ HH.span
            [ css_ "icon"]
            [ HH.i
              [ css_ "mdi  mdi-chevron-right"]
              []
            ]
          ]
        ]
      , HH.ul_
        [ HH.li_
          [ HH.a
            []
            [ HH.text "America" ]
          ]
        ]
      , HH.div
        [ css_ "menu is-menu-main" ]
        [ HH.p
          [ css_ "menu-label" ]
          [ HH.text "User" ]
        ]
      ]
    , HH.ul
      [ css_ "menu-list" ]
      [ HH.li_
        [ HH.a
          [ css_ ("has-icon" <> guard (route == Settings ) " is-active")
          , safeHref Settings
          ]
          [ HH.span
            [ css_ "icon" ]
            [ HH.i
              [ css_ "mdi menu mdi-settings" ]
              []
            ]
          , HH.span
            [ css_ "menu-item-label" ]
            [ HH.text "Settings" ]
          ]
        ]
      ]
    ]
  ]
