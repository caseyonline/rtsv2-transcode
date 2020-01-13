-- | This module exports a pure HTML function to render a consistent header throughout the app.
module Rtsv2App.Component.HTML.Header where

import Prelude

import Data.Maybe (Maybe, isNothing, isJust)
import Data.Monoid (guard)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Rtsv2App.Component.HTML.Utils (css, maybeElem, safeHref, whenElem)
import Rtsv2App.Data.Avatar as Avatar
import Rtsv2App.Data.Profile (ProfileRep)
import Rtsv2App.Data.Route (Route(..))
import Rtsv2App.Data.Username as Username

-- | Our header will be a pure render function, but we'll require a route as an argument so we can 
-- | judge whether a link should display active or not. We'll allow for any profile record type so 
-- | long as it has our core fields -- this makes the header reusable across pages despite which 
-- | variation on `Profile` they use.
header :: forall i p r. Maybe { | ProfileRep r } -> Route -> HH.HTML i p
header currentUser route =
  HH.nav
    [ css "header-navbar navbar-expand-md navbar navbar-with-menu navbar-without-dd-arrow fixed-top navbar-semi-light" ]
    [ HH.div
      [ css "navbar-wrapper" ]
      [ HH.div
        [ css "collapse navbar-collapse show"
        , HP.id_ "navbar-mobile"]
        []
      ]
    ]



    --   , [ HH.div
    --       [css "main-menu-content"]
    --       [ HH.li
    --         [ css "navbar-brand"
    --         , safeHref Home
    --         ]
    --         [ HH.text "conduit" ]
    --       , HH.li
    --         [ css "nav navbar-nav pull-xs-right" ]
    --         [ navItem Home
    --             [ HH.text "Home" ]
    --         , whenElem (isJust currentUser) \_ ->
    --             navItem Editor
    --               [ HH.i
    --                 [ css "ion-compose" ]
    --                 [ HH.text " New Post" ]
    --               ]
    --         , whenElem (isJust currentUser) \_ ->
    --             navItem Settings
    --               [ HH.i
    --                 [ css "ion-gear-a" ]
    --                 [ HH.text " Settings" ]
    --               ]
    --         , maybeElem currentUser \profile ->
    --             navItem (Profile profile.username)
    --               [ HH.img
    --                 [ css "user-pic"
    --                 , HP.src $ Avatar.toStringWithDefault profile.image
    --                 ]
    --               , HH.text $ Username.toString profile.username
    --               ]
    --         , whenElem (isNothing currentUser) \_ ->
    --             navItem Login
    --               [ HH.text "Log in" ]
    --         , whenElem (isNothing currentUser) \_ ->
    --             navItem Register
    --               [ HH.text "Sign up" ]
    --         ]
    --       ]
    --     ]
    --   ]
    -- ]
