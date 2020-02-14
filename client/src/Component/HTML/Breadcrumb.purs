module Rtsv2App.Component.HTML.Breadcrumb where

import Halogen.HTML as HH
import Rtsv2App.Component.HTML.Utils (css_)

-- | Breadcrum takes an array of HH.li's
component :: forall p i. Array (HH.HTML p i) -> HH.HTML p i
component html =
  HH.section
  [ css_ "section is-title-bar" ]
  [ HH.div
    [ css_ "level" ]
    [ HH.div
      [ css_ "level-left" ]
      [ HH.div
        [ css_ "level-item" ]
        [ HH.ul_
          html
        ]
      ]
    ]
  ]
