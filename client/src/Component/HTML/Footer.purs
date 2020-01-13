-- | This module exports a pure HTML function to render a consistent footer throughout the app.
module Rtsv2App.Component.HTML.Footer where

import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Rtsv2App.Component.HTML.Utils (css)

footer :: forall i p. HH.HTML i p
footer = 
  HH.footer
  [ css "footer footer-static footer-light navbar-border navbar-shadow" ]
  [ HH.div
    [ css "clearfix blue-grey lighten-2 text-sm-center mb-0 px-2" ]
    [ HH.span
      [ css "float-md-left d-block d-md-inline-block" ]
      [ HH.text "2020 © Copyright" ]
    , HH.span
      [ css "attribution" ]
      [ HH.text "An interactive Admin area by "
      , HH.a
        [ HP.href "https://www.id3as.com/" ]
        [ HH.text "Id3as" ]
      , HH.text "."
      ]
    ]
  ]
