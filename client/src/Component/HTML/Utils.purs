module Rtsv2App.Component.HTML.Utils
       ( css_
       , css
       , dataAttr
       , safeHref
       , printHrefPoP
       , maybeElem
       , whenElem
       ) where

import Prelude

import Data.Maybe (Maybe(..))
import Halogen.HTML as HH
import Halogen.HTML.Core (AttrName(..))
import Halogen.HTML.Properties as HP
import Routing.Duplex (print)
import Rtsv2App.Data.Route (Route, routeCodec)

-------------------------------------------------------------------------------
-- Useful Halogen Utils
-------------------------------------------------------------------------------

-- | This small utility saves a few characters all over our HTML.
css_ :: forall r i. String -> HH.IProp ( class :: String | r ) i
css_ = HP.class_ <<< HH.ClassName


css :: forall p i. Array String -> HH.IProp (class :: String | i) p
css = HP.classes <<< map HH.ClassName


-- | custom data attribute pass name and it's value as `String`s
dataAttr :: forall r i. String -> String -> HH.IProp r i
dataAttr atrName atrVal = HP.attr (AttrName $ "data-" <> atrName) atrVal

-- | We must provide a `String` to the "href" attribute, but we represent routes with the much
-- | better `Route` type. This utility is a drop-in replacement for `href` that uses `Route`.
safeHref :: forall r i. Route -> HH.IProp ( href :: String | r) i
safeHref = HP.href <<< append "#" <<< print routeCodec

-- | this is used to turn PoPR into a string to be used as links in a map on Dashboard
printHrefPoP :: Route -> String
printHrefPoP r = "/app/#" <> print routeCodec r <> "/"

-- | Sometimes we need to deal with elements which may or may not exist. This function lets us
-- | provide rendering for the element if it exists, and renders an empty node otherwise.
maybeElem :: forall p i a. Maybe a -> (a -> HH.HTML p i) -> HH.HTML p i
maybeElem (Just x) f = f x
maybeElem _ _ = HH.text ""

-- | PureScript is a strict language. If we want to conditionally display an element, then we
-- | should hide the evaluation behind a function, which won't be evaluated right away, in order
-- | to minimize the work performed each render.
whenElem :: forall p i. Boolean -> (Unit -> HH.HTML p i) -> HH.HTML p i
whenElem cond f = if cond then f unit else HH.text ""
