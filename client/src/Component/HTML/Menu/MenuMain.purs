module Rtsv2App.Component.HTML.Menu.MenuMain where

import Prelude

import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Data.Monoid (guard)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Rtsv2App.Capability.Navigate (class Navigate, logout)
import Rtsv2App.Component.HTML.Menu.MainHelper (MenuState, closeSecondaryMenu, isSecondaryClosed, openSecondaryMenu)
import Rtsv2App.Component.HTML.Utils (css_, safeHref)
import Rtsv2App.Data.Profile (Profile)
import Rtsv2App.Data.Route (Route(..))
import Shared.Rtsv2.Types (PoPName)

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------
data Action
  = LogUserOut
  | Receive Input
  | CloseSecondaryMenu

type State =
  { currentUser  :: Maybe Profile
  , route        :: Route
  , isMenuClosed :: Boolean
  , curPopName   :: Maybe PoPName
  }

type Input =
  { currentUser  :: Maybe Profile
  , route        :: Route
  , isMenuClosed :: Boolean
  , curPopName   :: Maybe PoPName
  }

type Slot
  = H.Slot (Const Void) MenuState 


-------------------------------------------------------------------------------
-- Component
-------------------------------------------------------------------------------
component
  :: forall m
   . MonadAff m
  => Navigate m
  => H.Component HH.HTML (Const Void) Input MenuState m
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
  initialState { currentUser, route, isMenuClosed, curPopName } =
    { currentUser
    , route
    , isMenuClosed
    , curPopName
    }

  handleAction = case _ of
    Receive { currentUser, route, isMenuClosed, curPopName } -> do
      H.put { currentUser
            , route
            , isMenuClosed
            , curPopName 
            }

    LogUserOut -> logout

    CloseSecondaryMenu -> do
      maybeIsMenuClosed <- liftEffect $ isSecondaryClosed
      case maybeIsMenuClosed of
        Nothing           -> pure unit
        Just isMenuClosed ->
          if isMenuClosed
            then do
              H.modify_ _ { isMenuClosed = false }
              _ <- liftEffect $ openSecondaryMenu
              pure unit
            else do
              H.modify_ _ { isMenuClosed = true }
              _ <- liftEffect $ closeSecondaryMenu
              pure unit
      

  render state@{ currentUser, route, isMenuClosed, curPopName } =
    HH.aside
    [ css_ "aside is-placed-left is-expanded"
    , HP.id_ "aside-main"  
    ]
    [ topLogo
    , menuItems state
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

-------------------------------------------------------------------------------
-- Logo Top title
-------------------------------------------------------------------------------
topLogo :: forall p i. HH.HTML p i
topLogo =
  HH.div
  [ css_ "aside-tools" ]
  [ HH.div
    [ css_ "aside-tools-label" ]
    [ HH.span_
      [ HH.a
        [ safeHref DashboardR ]
        [ HH.span
          [ css_ "icon is-medium is-aligned-center" ]
          [ HH.img
            [ css_ "menu-logo"
            , HP.src "support/assets/images/logo/logo.svg"
            ]
          ]
        ]
      ]
    ]
  ]

-------------------------------------------------------------------------------
-- Menu Items
-------------------------------------------------------------------------------
menuItems :: forall p. State -> HH.HTML p Action
menuItems { route, isMenuClosed, curPopName } =
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
        [ if isMenuClosed
             then css_ ("has-icon" <> guard (route == DashboardR) " is-active")
             else css_ ("has-icon")
        , safeHref DashboardR
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
        [ css_ ("has-icon has-submenu-icon jb-aside-secondary-toggle" <>
          (if isMenuClosed
             then case curPopName of
                    Nothing    -> ""
                    Just pName -> (guard (route == PoPDashboardR pName ) " is-active")
             else " is-active"
          ))
        , HE.onClick \_ -> Just CloseSecondaryMenu
        ]
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
          [ css_ ("has-icon" <> guard (route == SettingsR ) " is-active")
          , safeHref SettingsR
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
