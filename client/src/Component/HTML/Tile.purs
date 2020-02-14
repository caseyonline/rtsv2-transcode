module Rtsv2App.Component.HTML.Tile where

import Prelude

import Halogen.HTML as HH
import Rtsv2App.Component.HTML.Utils (css_)


type TyleInput =
  { headerIconType :: String
  , headerText     :: String
  , subTitleH3Text :: String
  , subTitleH1Text :: String
  , widgetTextType :: String
  , widgetIconType :: String
  }

component :: forall p i. TyleInput -> HH.HTML p i
component i =
  HH.div
  [ css_ "tile is-parent" ]
  [ HH.div
    [ css_ "card is-card-widget tile is-child" ]
    [ HH.header
      [ css_ "card-header" ]
      [ HH.p
        [ css_ "card-header-title" ]
        [ HH.span
          [ css_ "icon" ]
          [ HH.i
            [ css_ $ "mdi " <> i.headerIconType ]
            []
          ]
        , HH.span_
          [ HH.text i.headerText ]
        ]
      ]
    , HH.div
      [ css_ "card-content" ]
      [ HH.div
        [ css_ "level is-mobile" ]
        [ HH.div
          [ css_ "level-item" ]
          [ HH.div
            [ css_ "is-widget-label" ]
            [ HH.h3
              [ css_ "subtitle is-spaced" ]
              [ HH.text i.subTitleH3Text ]
            , HH.h1
              [css_ "title" ]
              [ HH.text i.subTitleH1Text ]
            ]
          ]
        , HH.div
          [ css_ "level-item has-widget-icon" ]
          [ HH.div
            [ css_ "is-widget-icon" ]
            [ HH.span
              [ css_ $ "icon is-large " <> i.widgetTextType ]
              [ HH.i
                [ css_ $ "mdi mdi-48px " <> i.widgetIconType ]
                []
              ]
            ]
          ]
        ]
      ]
    ]
  ]
