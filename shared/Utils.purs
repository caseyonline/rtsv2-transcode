module Shared.Utils where

import Prelude

import Partial.Unsafe (unsafeCrashWith)

lazyCrashIfMissing :: forall a. String -> Unit -> a
lazyCrashIfMissing s = \unit -> unsafeCrashWith s
