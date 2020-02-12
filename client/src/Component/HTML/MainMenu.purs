module Rtsv2App.Component.HTML.MainMenu where

import Prelude

import Data.Const (Const)
import Data.Maybe (Maybe(..), isJust)
import Data.Monoid (guard)
import Data.Newtype (unwrap)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
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
    HH.div
    [ css_ "main-menu menu-fixed menu-light menu-accordion menu-shadow"
    , dataAttr "scroll-to-active" "true"
    , dataAttr "img" ""
    ]
    [ HH.div
      [ css_ "navbar-header" ]
      [ HH.ul
        [ css_ "nav navbar-nav flex-row" ]
        [ HH.li
          [ css_ "nav-item mr-auto" ]
          [ HH.a
            [ css_ "navbar-brand"
            , safeHref Dashboard
            ]
            [ HH.img
              [ css_ "brand-logo"
              , HP.src "assets/images/logo/logo.png"
              ]
            , HH.h3
                [css_ "brand-text"]
                [ HH.text "Limelight Admin" ]
            ]
          ]
        ,  HH.li
           [ css_ "nav-item d-md-none" ]
           [ HH.a
             [ css_ "nav-link close-navbar" ]
             [ HH.i
               [ css_ "ft-x" ]
               []
             ]
           ]
        ]
      ]
    , HH.div
      [ css_ "main-menu-content" ]
      [ HH.ul
        [ css_ "navigation navigation-main"
        , dataAttr "data-menu" "menu-navigation"
        , HP.id_ "main-menu-navigation"
        ]
        [ navItem Dashboard
          [ HH.i
            [css_ "ft-home"]
            []
          , HH.span
            [css_ "menu-title"]
            [ HH.text "Dashboard" ]
          ]
        , HH.li
            [ css_ $ "nav-item has-sub " <> guard (route == (PoPHome "") ) "active"  ]
            [ HH.a
              []
              [ HH.i
                [ css_ "ft-server" ]
                []
              , HH.span
                [css_ "menu-title"]
                [ HH.text "PoP" ]
              ]
            , HH.ul
              [ css_ "menu-content"]
              [ HH.li
                [ css_ "has-sub" ]
                [ HH.a
                  [ css_ "menu-item" ]
                  [ HH.text "America" ]
                , HH.ul
                  [ css_ "menu-content" ]
                  [ HH.li
                    []
                    [ HH.a
                      [ css_ "menu-item" ]
                      [ HH.text "poop" ]
                    ]

                  ]
                ]
              ]
            ]
         , navItem Settings
           [ HH.i
             [css_ "ft-settings"]
             []
           , HH.span
             [css_ "menu-title"]
             [ HH.text "Settings" ]
           ]

         , whenElem (not $ isJust currentUser) \_ ->
             navItem Login
             [ HH.i
               [css_ "ft-lock"]
               []
             , HH.span
               [css_ "menu-title"]
               [ HH.text "Login" ]
             ]
         , whenElem (isJust currentUser) \_ ->
           HH.li
           []
           [ HH.a
             [ HE.onClick \_ -> Just LogUserOut ]
             [ HH.i
               [css_ "ft-lock"]
               []
             , HH.span
               [css_ "menu-title"]
               [ HH.text "Logout" ]
             ]
           ]
         ]
      ]
    ]
    where
    navItem r html =
      HH.li
        [css_ $ "nav-item " <> guard (route == r) "active"  ]
        [ HH.a
          [ safeHref r ]
          html
        ]
