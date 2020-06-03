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
       , doLogCommand
       , addLoggerMetadata
       , Logger
       , EventType(..)
       , LogType(..)
       , MinimalMetadata
       , class SpyWarning
       ) where

import Prelude

import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import Erl.Atom (Atom, atom)
import Erl.Data.List (List, singleton)
import Prim.Row as Row
import Prim.TypeError (class Warn, Text)
import Shared.Common (LoggingMetadata)

type Logger metadata report = metadata -> report -> Effect Unit

data LogType = Trace
             | Event
             | Command
             | Audit

data EventType = Start
               | Stop

type MinimalMetadata a =
  { domain :: List Atom
  , "type" :: LogType
  | a
  }

type TraceMetadata = MinimalMetadata ( text :: String )

type EventMetadata = MinimalMetadata ( event :: EventType
                                     , text :: String
                                     )

-- TODO - FFI error if we use the type alias
foreign import emergency :: forall metadata report. MinimalMetadata metadata -> { | report } -> Effect Unit
foreign import alert     :: forall metadata report. MinimalMetadata metadata -> { | report } -> Effect Unit
foreign import critical  :: forall metadata report. MinimalMetadata metadata -> { | report } -> Effect Unit
foreign import error     :: forall metadata report. MinimalMetadata metadata -> { | report } -> Effect Unit
foreign import warning   :: forall metadata report. MinimalMetadata metadata -> { | report } -> Effect Unit
foreign import notice    :: forall metadata report. MinimalMetadata metadata -> { | report } -> Effect Unit
foreign import info      :: forall metadata report. MinimalMetadata metadata -> { | report } -> Effect Unit
foreign import debug     :: forall metadata report. MinimalMetadata metadata -> { | report } -> Effect Unit
foreign import spyImpl   :: forall metadata report. MinimalMetadata metadata -> { | report } -> Effect Unit
foreign import addLoggerMetadata :: LoggingMetadata -> Effect Unit

class SpyWarning
instance warn :: Warn (Text "Logger.spy usage") => SpyWarning

spy :: forall a. SpyWarning => String -> a -> a
spy str a = unsafePerformEffect do
  _ <-  spyImpl { domain: singleton $ atom "spy"
                , "type": Trace
                , text: str} {spydata: a}
  pure a

doLog :: forall report. Row.Lacks "text" report => List Atom -> Logger TraceMetadata { | report} -> String -> { | report} -> Effect Unit
doLog domain logger msg report =
  logger { domain
         , "type": Trace
         , text: msg} report

doLogEvent :: forall report. Row.Lacks "text" report => List Atom -> EventType -> Logger EventMetadata { | report} -> String -> { | report} -> Effect Unit
doLogEvent domain event logger msg report =
  logger { domain
         , "type": Event
         , event
         , text: msg} report

doLogCommand :: forall report. Row.Lacks "text" report => List Atom -> String -> { | report} -> Effect Unit
doLogCommand domain msg report =
  notice { domain
         , "type": Command
         , text: msg} report
