-- | This module exports a pure HTML function to render a consistent header throughout the app.
module Rtsv2App.Component.HTML.MainMenu where

import Prelude

import Data.Maybe (Maybe(..), isNothing, isJust)
import Data.Monoid (guard)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Rtsv2App.Component.HTML.Utils (css, dataAttr, maybeElem, safeHref, whenElem)
import Rtsv2App.Data.Avatar as Avatar
import Rtsv2App.Data.Profile (ProfileRep)
import Rtsv2App.Data.Route (Route(..))
import Rtsv2App.Data.Username as Username

-- | Our header will be a pure render function, but we'll require a route as an argument so we can 
-- | judge whether a link should display active or not. We'll allow for any profile record type so 
-- | long as it has our core fields -- this makes the header reusable across pages despite which 
-- | variation on `Profile` they use.
mainMenu :: forall i p r. Maybe { | ProfileRep r } -> Route -> HH.HTML i p
mainMenu currentUser route =
   HH.div
    [ css "main-menu menu-fixed menu-light menu-accordion menu-shadow "
    , dataAttr "scroll-to-active" "true"
    , dataAttr "img" "assets/images/backgrounds/02.jpg"]
    [ HH.div
      [ css "navbar-header" ]
      [ HH.ul
        [ css "nav navbar-nav flex-row" ]
        [ HH.li
          [ css "nav-item mr-auto" ]
          [ HH.a
            [ css "navbar-brand"
            , safeHref Home
            ]
            [ HH.img
              [ css "brand-logo"
              , HP.src "assets/images/logo/logo.png"
              ]
            , HH.h3
                [css "brand-text"]
                [ HH.text "Limelight Admin" ]
            ]
          ]
        ,  HH.li
           [ css "nav-item d-md-none" ]
           [ HH.a
             [ css "nav-link close-navbar" ]
             [ HH.i
               [ css "ft-x" ]
               []
             ]
           ]
        ]
      ]
    , HH.div
      [ css "main-menu-content" ]
      [ HH.ul
        [ css "navigation navigation-main"
        , HP.id_ "main-menu-navigation"
        ]
        [ navItem Home
          [ HH.i
            [css "ft-home"]
            []
          , HH.span
            [css "menu-title"]
            [ HH.text "Dashboard" ]
          ]
         , navItem Login
           [ HH.i
             [css "ft-lock"]
             []
           , HH.span
             [css "menu-title"]
             [ HH.text "Login" ]
           ]
         , whenElem (isJust currentUser) \_ ->
           navItem Editor
           [ HH.i
             [css "ft-settings"]
             []
           , HH.span
             [css "menu-title"]
             [ HH.text "Settings" ]
           ]
         , navItem Register
           [ HH.i
             [css "ft-video"]
             []
           , HH.span
             [css "menu-title"]
             [ HH.text "Stream" ]
           ]
         , whenElem (isJust currentUser) \_ ->
           HH.li
           []
           [ HH.a_
             [ HH.i
               [css "ft-lock"]
               []
             , HH.span
               [css "menu-title"]
               [ HH.text "Logout" ]
             ]
           ]
         ]
      ]
    ]
  where
  navItem r html =
      HH.li
        [css $ "" <> guard (route == r) "active"  ]
        [ HH.a
          [ safeHref r ]
          html
        ]
