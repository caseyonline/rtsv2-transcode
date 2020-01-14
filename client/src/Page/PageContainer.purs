module Rtsv2App.Page.PageContainer (pageContainer) where

import Data.Maybe (Maybe)
import Halogen.HTML as HH
import Rtsv2App.Component.HTML.Footer (footer)
import Rtsv2App.Component.HTML.Header (header)
import Rtsv2App.Component.HTML.MainMenu (mainMenu)
import Rtsv2App.Component.HTML.Utils (css)
import Rtsv2App.Data.Profile (ProfileRep)
import Rtsv2App.Data.Route (Route)

pageContainer :: forall i p r. Maybe { | ProfileRep r } -> Route -> Array (HH.HTML i p) -> HH.HTML i p
pageContainer currentUser route html =
  HH.div_
      [ header currentUser route
      , mainMenu currentUser route
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
                [ HH.text "Login" ]
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
