module Shared.Utils where

import Prelude

import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), split)
import Data.Traversable (sequence)
import Partial.Unsafe (unsafeCrashWith)
import Serf (Ip(..))

lazyCrashIfMissing :: forall a. String -> Unit -> a
lazyCrashIfMissing s = \unit -> unsafeCrashWith s

strToIp :: String -> Maybe Ip
strToIp str =
  str
  # split (Pattern ".")
  # map fromString
  # sequence
  # (\ip -> case ip of
        Just [a, b, c, d] -> Just (Ipv4 a b c d)
        _ -> Nothing
    )
