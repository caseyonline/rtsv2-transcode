module Erl.Utils
       ( systemTimeMs
       , vmTimeMs
       , sleep
       , makeRef
       , privDir
       , self
       , shutdown
       , trapExit
       , mapExitReason
       , exitMessageMapper
       , Ref
       , ExitReason(..)
       , ExitMessage(..)
       )
       where

import Prelude

import Data.Maybe (Maybe)
import Data.Newtype (unwrap, wrap)
import Effect (Effect)
import Erl.Atom (Atom, atom)
import Erl.Process.Raw (Pid)
import Foreign (Foreign)
import Shared.Common (Milliseconds)

foreign import systemTimeImpl :: Atom -> Effect Int
foreign import vmTimeImpl :: Atom -> Effect Int
foreign import sleepImpl :: Int -> Effect Unit
foreign import makeRefImpl :: Effect Ref
foreign import privDirImpl :: Atom -> String
foreign import eqRefImpl :: Ref -> Ref -> Boolean
foreign import data Ref :: Type
foreign import selfImpl :: Effect Pid
foreign import trapExitImpl :: Boolean -> Effect Boolean
foreign import mapExitReasonImpl :: Foreign -> ExitReason
foreign import shutdownImpl :: Pid -> Effect Unit
foreign import exitMessageMapperImpl :: Foreign -> Maybe ExitMessage

data ExitReason = Normal
                | Shutdown Foreign
                | Other Foreign

data ExitMessage = Exit Pid Foreign

sleep :: Milliseconds -> Effect Unit
sleep = sleepImpl <<< unwrap

-- TODO - find a place for these utility types to live (a la id3as_common?)

systemTimeMs :: Effect Milliseconds
systemTimeMs = wrap <$> systemTimeImpl (atom "millisecond")

vmTimeMs :: Effect Milliseconds
vmTimeMs = wrap <$> vmTimeImpl (atom "millisecond")

instance eqRef :: Eq Ref where
  eq = eqRefImpl

makeRef :: Effect Ref
makeRef = makeRefImpl

privDir :: Atom -> String
privDir = privDirImpl

trapExit :: Boolean -> Effect Boolean
trapExit = trapExitImpl

self :: Effect Pid
self = selfImpl

shutdown :: Pid -> Effect Unit
shutdown = shutdownImpl

mapExitReason :: Foreign -> ExitReason
mapExitReason = mapExitReasonImpl

exitMessageMapper :: Foreign -> Maybe ExitMessage
exitMessageMapper = exitMessageMapperImpl
