module Logger
       ( emergency
       , alert
       , critical
       , error
       , warning
       , notice
       , info
       , debug
       , spy
       , doLog
       , Logger
       , class SpyWarning
       ) where

import Prelude

import Prim.TypeError (class Warn, Text)
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import Erl.Atom (Atom)
import Erl.Data.List (List)


type Logger a = String -> a -> Effect Unit
-- TODO - FFI error if we use the type alias
foreign import emergency :: forall a. String -> a -> Effect Unit
foreign import alert     :: forall a. String -> a -> Effect Unit
foreign import critical  :: forall a. String -> a -> Effect Unit
foreign import error     :: forall a. String -> a -> Effect Unit
foreign import warning   :: forall a. String -> a -> Effect Unit
foreign import notice    :: forall a. String -> a -> Effect Unit
foreign import info      :: forall a. String -> a -> Effect Unit
foreign import debug     :: forall a. String -> a -> Effect Unit
foreign import spyImpl   :: forall a. String -> a -> Effect Unit


class SpyWarning
instance warn :: Warn (Text "Logger.spy usage") => SpyWarning

spy :: forall a. SpyWarning => String -> a -> a
spy str a = unsafePerformEffect do
  _ <-  spyImpl str {misc : a}
  pure a

doLog :: forall a. List Atom -> Logger {domain :: List Atom, misc :: a} -> Logger a
doLog domain logger msg misc =
  logger msg { domain: domain, misc: misc }
