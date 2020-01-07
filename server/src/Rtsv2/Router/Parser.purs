module Rtsv2.Router.Parser (printUrl) where

import Prelude

import Data.Function (applyFlipped)
import Data.Newtype (unwrap)
import Data.String (joinWith)
import Data.String.CodeUnits (contains)
import Data.String.Pattern (Pattern(..))
import Data.Tuple (uncurry)
import Global.Unsafe (unsafeEncodeURIComponent)
import Routing.Duplex (RouteDuplex(..))
import Routing.Duplex.Printer (RoutePrinter)
import Routing.Duplex.Types (RouteState, emptyRouteState)

-- | custome route printer to handle the ability to pass `:` to Stretson.route
printUrl :: forall i o. RouteDuplex i o -> i -> String
printUrl (RouteDuplex enc _) = run <<< enc

run :: RoutePrinter -> String
run = printPath <<< applyFlipped emptyRouteState <<< unwrap

printPath :: RouteState -> String
printPath { segments, params, hash: hash' } =
  printSegments segments <> printParams params <> printHash hash'
  where
  printSegments = case _ of
    [""] -> "/"
    xs -> joinWith "/" $ map (\a -> if contains (Pattern ":") a
                                      then a
                                      else unsafeEncodeURIComponent a) xs

  printParams [] = ""
  printParams ps = "?" <> joinWith "&" (uncurry printParam <$> ps)

  printParam key ""  = unsafeEncodeURIComponent key
  printParam key val = unsafeEncodeURIComponent key <> "=" <> unsafeEncodeURIComponent val

  printHash "" = ""
  printHash h  = "#" <> h
