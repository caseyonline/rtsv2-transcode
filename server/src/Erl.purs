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
       , monitor
       , link
       , mapExitReason
       , exitMessageMapper
       , base64Encode
       , readTuple2
       , whereis
       , monotonicTime
       , Ref
       , ExitReason(..)
       , ExitMessage(..)
       , MonotonicTime
       )
       where

import Prelude

import Control.Monad.Except (except)
import Data.Either (Either(..), note)
import Data.List.NonEmpty (singleton)
import Data.List.NonEmpty as NEL
import Data.Long as Long
import Data.Maybe (Maybe, maybe)
import Data.Maybe as Maybe
import Data.Newtype (unwrap, wrap)
import Effect (Effect)
import Erl.Atom (Atom, atom)
import Erl.Data.Tuple (Tuple2, tuple2, tuple3)
import Erl.ModuleName (NativeModuleName(..))
import Erl.Process.Raw (Pid)
import Foreign (Foreign, ForeignError(..), F, tagOf, unsafeToForeign)
import Pinto (ServerName(..))
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
foreign import whereisImpl :: Foreign -> Effect (Maybe Pid)
foreign import mapExitReasonImpl :: Foreign -> ExitReason
foreign import shutdownImpl :: Pid -> Effect Unit
foreign import exitMessageMapperImpl :: Foreign -> Maybe ExitMessage
foreign import refToStringImpl :: Ref -> Foreign
foreign import stringToRefImpl :: Foreign -> Maybe Ref
foreign import monitorImpl :: Foreign -> Effect Unit
foreign import linkImpl :: Pid -> Effect Unit
foreign import monotonicTime :: Effect MonotonicTime

data ExitReason = Normal
                | Shutdown Foreign
                | Other Foreign

data ExitMessage = Exit Pid Foreign

newtype MonotonicTime = MonotonicTime Int
derive instance eqMonotonicTime :: Eq MonotonicTime


sleep :: Milliseconds -> Effect Unit
sleep = sleepImpl <<< Maybe.fromMaybe 0 <<<  Long.toInt <<< unwrap

-- TODO - find a place for these utility types to live (a la id3as_common?)

systemTimeMs :: Effect Milliseconds
systemTimeMs = wrap <$> Long.fromInt <$> systemTimeImpl (atom "millisecond")

vmTimeMs :: Effect Milliseconds
vmTimeMs = wrap <$> Long.fromInt <$> vmTimeImpl (atom "millisecond")

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

monitor :: forall a b. ServerName a b -> Effect Unit
monitor (Local name) = monitorImpl $ unsafeToForeign $ name
monitor (Global name) = monitorImpl $ unsafeToForeign $ tuple2 (atom "global") name
monitor (Via (NativeModuleName m) name) = monitorImpl $ unsafeToForeign $ tuple3 (atom "via") m name

link :: Pid -> Effect Unit
link = linkImpl

shutdown :: Pid -> Effect Unit
shutdown = shutdownImpl

foreign import readTuple2Impl :: Foreign -> Maybe (Tuple2 Foreign Foreign)

readTuple2 :: Foreign -> F (Tuple2 Foreign Foreign)
readTuple2 f = except $ note (singleton $ ForeignError "invalid tuple") $ readTuple2Impl f

whereis :: Foreign -> Effect (Maybe Pid)
whereis = whereisImpl

mapExitReason :: Foreign -> ExitReason
mapExitReason = mapExitReasonImpl

exitMessageMapper :: Foreign -> Maybe ExitMessage
exitMessageMapper = exitMessageMapperImpl

foreign import base64EncodeImpl :: String -> String

base64Encode :: String -> String
base64Encode = base64EncodeImpl

exit :: Foreign -> Effect Unit
exit = exitImpl
