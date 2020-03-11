module Erl.Utils
       ( systemTimeMs
       , vmTimeMs
       , sleep
       , makeRef
       , privDir
       , self
       , shutdown
       , trapExit
       , exit
       , mapExitReason
       , exitMessageMapper
       , Ref
       , ExitReason(..)
       , ExitMessage(..)
       )
       where

import Prelude

import Control.Monad.Except (except)
import Data.Either (Either(..))
import Data.List.NonEmpty as NEL
import Data.Maybe (Maybe, maybe)
import Data.Newtype (unwrap, wrap)
import Effect (Effect)
import Erl.Atom (Atom, atom)
import Erl.Process.Raw (Pid)
import Foreign (Foreign, ForeignError(..), tagOf)
import Shared.Common (Milliseconds)
import Simple.JSON (class ReadForeign, class WriteForeign)

foreign import systemTimeImpl :: Atom -> Effect Int
foreign import vmTimeImpl :: Atom -> Effect Int
foreign import sleepImpl :: Int -> Effect Unit
foreign import makeRefImpl :: Effect Ref
foreign import privDirImpl :: Atom -> String
foreign import eqRefImpl :: Ref -> Ref -> Boolean
foreign import data Ref :: Type
foreign import selfImpl :: Effect Pid
foreign import trapExitImpl :: Boolean -> Effect Boolean
foreign import exitImpl :: Foreign -> Effect Unit
foreign import mapExitReasonImpl :: Foreign -> ExitReason
foreign import shutdownImpl :: Pid -> Effect Unit
foreign import exitMessageMapperImpl :: Foreign -> Maybe ExitMessage
foreign import refToStringImpl :: Ref -> Foreign
foreign import stringToRefImpl :: Foreign -> Maybe Ref

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

instance eqRef :: Eq Ref where eq = eqRefImpl
instance writeForeignRef :: WriteForeign Ref where writeImpl = refToStringImpl
instance readForeignRef :: ReadForeign Ref where
  readImpl f =
    let
      fromRef = maybe error pure <<< stringToRefImpl
      error = Left $ NEL.singleton $ TypeMismatch "Ref" (tagOf f)
    in
     except (fromRef f)

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

exit :: Foreign -> Effect Unit
exit = exitImpl
