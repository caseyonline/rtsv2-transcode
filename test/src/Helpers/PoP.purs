module Helpers.PoP where

import Prelude

import Helpers.Types (PoPInfo)


makePoPInfo :: String -> Int -> PoPInfo
makePoPInfo n i = {name: n, number: i, x: 0.0, y: 0.0}
