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
       , doLogEvent
       , Logger
       , EventType(..)
       , class SpyWarning
       ) where

import Prelude

import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import Erl.Atom (Atom)
import Erl.Data.List (List)
import Prim.Row as Row
import Prim.TypeError (class Warn, Text)

type Logger metadata report = metadata -> report -> Effect Unit

-- TODO - FFI error if we use the type alias
foreign import emergency :: forall metadata report. { | metadata } -> { | report } -> Effect Unit
foreign import alert     :: forall metadata report. { | metadata } -> { | report } -> Effect Unit
foreign import critical  :: forall metadata report. { | metadata } -> { | report } -> Effect Unit
foreign import error     :: forall metadata report. { | metadata } -> { | report } -> Effect Unit
foreign import warning   :: forall metadata report. { | metadata } -> { | report } -> Effect Unit
foreign import notice    :: forall metadata report. { | metadata } -> { | report } -> Effect Unit
foreign import info      :: forall metadata report. { | metadata } -> { | report } -> Effect Unit
foreign import debug     :: forall metadata report. { | metadata } -> { | report } -> Effect Unit
foreign import spyImpl   :: forall metadata report. { | metadata } -> { | report } -> Effect Unit

class SpyWarning
instance warn :: Warn (Text "Logger.spy usage") => SpyWarning

data EventType = Start
               | Stop

spy :: forall a. SpyWarning => String -> a -> a
spy str a = unsafePerformEffect do
  _ <-  spyImpl {text: str} {spydata: a}
  pure a

doLog :: forall report. Row.Lacks "text" report => List Atom -> Logger {domain :: List Atom, text :: String} { | report} -> String -> { | report} -> Effect Unit
doLog domain logger msg report =
  logger { domain
         , text: msg} report

doLogEvent :: forall report. Row.Lacks "text" report => List Atom -> EventType -> Logger {domain :: List Atom, event :: EventType, text :: String} { | report} -> String -> { | report} -> Effect Unit
doLogEvent domain event logger msg report =
  logger { domain
         , event
         , text: msg} report
