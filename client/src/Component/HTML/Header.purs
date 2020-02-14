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
    [ css_ "navbar is-fixed-top"
    , HP.id_ "navbar-main"
    ]
    [
    -- [ HH.div
    --   [ css_ "navbar-brand" ]
    --   [ HH.a
    --     [ css_ "navbar-item is-desktop-icon-only is-hidden-touch jb-aside-toggle"
    --     , dataAttr "target" "aside-main" ]
    --     [ HH.span
    --       [ css_ "icon" ]
    --       [ HH.i
    --         [ css_ "mdi mdi-24px mdi-forwardburger" ]
    --         []
    --       ]
    --     ]
    --   ]
      HH.div
      [ css_ "navbar-menu fadeIn animated faster"
      , HP.id_ "navbar-menu"
      ]
      [ HH.div
        [ css_ "navbar-end" ]
        [ HH.div
          [ css_ "navbar-item has-dropdown has-dropdown-with-icons has-divider has-user-avatar is-hoverable" ]
          [ HH.a
            [ css_ "navbar-link is-arrowless" ]
            [ HH.div
              [ css_ "is-user-avatar" ]
              [ HH.img
                [ HP.src "assets/images/avatar-s.png" ]
              ]
            , HH.div
              [ css_ "is-user-name" ]
              [ HH.span
                [ css_ "icon" ]
                [ HH.i
                  [ css_ "mdi mdi-chevron-down" ]
                  []
                ]
              ]
            ]
          , HH.div
            [ css_ "navbar-dropdown"]
            [ HH.a
              [ css_ "navbar-item"
              , safeHref $ Settings
              ]
              [ HH.span
                [ css_ "icon" ]
                [ HH.i
                  [ css_ "mdi mdi-settings" ]
                  []
                ]
              , HH.span_
                [ HH.text "Settings" ]
              ]
            , HH.hr
              [ css_ "navbar-divider"]
            , HH.a
              [ css_ "navbar-item" ]
              [ HH.span
                [ css_ "icon" ]
                [ HH.i
                  [ css_ "mdi mdi-logout" ]
                  []
                ]
              , HH.span_
                [ HH.text "Log Out" ]
              ]
            ]
          ]
        , HH.a
          [ css_ "navbar-item is-desktop-icon-only" ]
          [ HH.span
            [ css_ "icon" ]
            [ HH.i
              [css_ "mdi mdi-logout"]
              []
            ]
          , HH.span_
            [ HH.text "Log Out" ]
          ]
        --   <a title="Log out" class="navbar-item is-desktop-icon-only">
        --   <span class="icon"><i class="mdi mdi-logout"></i></span>
        --   <span>Log out</span>
        -- </a>
        ]
      ]
    ]

-- <div class="aside-tools">
--       <div class="aside-tools-label">
--         <span><b>JB</b>O</span>
--       </div>
--     </div>
