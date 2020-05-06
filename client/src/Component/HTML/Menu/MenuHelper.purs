module Rtsv2App.Component.HTML.Menu.MainHelper where

import Prelude

import Data.Maybe (Maybe)
import Data.String (contains, replace)
import Data.String.Pattern (Pattern(..), Replacement(..))
import Data.Traversable (for, for_)
import Effect (Effect)
import Web.DOM.Element (Element)
import Web.DOM.Element (className, setClassName) as DOM
import Web.DOM.NonElementParentNode (getElementById) as DOM
import Web.HTML (window) as HTML
import Web.HTML.HTMLDocument (toNonElementParentNode) as HTML
import Web.HTML.Window (document) as HTML

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------
data MenuState
  = OpenMenu
  | CloseMenu


-------------------------------------------------------------------------------
-- Functions
-------------------------------------------------------------------------------
closeSecondaryMenu :: Effect Unit
closeSecondaryMenu = do
  -- | setup parent container
  document <- HTML.window >>= HTML.document
  let parent = HTML.toNonElementParentNode document

  -- | get the elements by id
  mainContainer <- DOM.getElementById "aside-main" parent
  secondaryContainer <- DOM.getElementById "aside-secondary" parent

  -- | remove or add classes to specific elements
  for_ mainContainer $ (\cont -> removeClass " has-secondary" cont)
  for_ secondaryContainer (\cont -> addClass " is-hidden" cont)

openSecondaryMenu :: Effect Unit
openSecondaryMenu = do
  -- | setup parent container
  document <- HTML.window >>= HTML.document
  let parent = HTML.toNonElementParentNode document

  -- | get the elements by id
  mainContainer <- DOM.getElementById "aside-main" parent
  secondaryContainer <- DOM.getElementById "aside-secondary" parent

  -- | remove or add classes to specific elements
  for_ mainContainer $ (\cont -> addClass " has-secondary" cont)
  for_ secondaryContainer (\cont -> removeClass " is-hidden" cont)

isSecondaryClosed :: Effect (Maybe Boolean)
isSecondaryClosed = do
  -- | setup parent container
  document <- HTML.window >>= HTML.document
  let parent = HTML.toNonElementParentNode document

  secondaryContainer <- DOM.getElementById "aside-secondary" parent

  for secondaryContainer (\cont -> do
                             curClass <- DOM.className cont
                             pure $ contains (Pattern "is-hidden") curClass
                         )

-------------------------------------------------------------------------------
-- Internal functions
-------------------------------------------------------------------------------
removeClass :: String -> Element -> Effect Unit
removeClass css cont = do
      mainClass <- DOM.className cont
      let newClass = replace (Pattern css) (Replacement "") mainClass
      DOM.setClassName newClass cont


addClass :: String -> Element -> Effect Unit
addClass css cont = do
  mainClass <- DOM.className cont
  DOM.setClassName ( mainClass <> css ) cont
