module Rtsv2.Handler.Load
       (
         load
       ) where

import Prelude

import Data.Either (hush)
import Data.Maybe (fromMaybe, isNothing)
import Effect (Effect)
import Erl.Cowboy.Req (ReadBodyResult(..), Req, method, readBody)
import Erl.Data.Binary (Binary)
import Erl.Data.Binary.IOData (IOData, fromBinary, toBinary)
import Erl.Data.List (nil, singleton, (:))
import Rtsv2.Handler.MimeType as MimeType
import Rtsv2.Load as Load
import Shared.Rtsv2.Types (CurrentLoad, minLoad)
import Simple.JSON (readJSON)
import Simple.JSON as JSON
import Stetson (HttpMethod(..), StetsonHandler)
import Stetson.Rest as Rest
import Unsafe.Coerce (unsafeCoerce)

type State =
  {
    load :: CurrentLoad
  }

load :: StetsonHandler State
load =
  Rest.handler init
  # Rest.allowedMethods (Rest.result (GET : POST : nil))
  # Rest.malformedRequest malformedRequest
  # Rest.contentTypesProvided (\req state -> Rest.result (singleton $ MimeType.json provideJson) req state)
  # Rest.contentTypesAccepted (\req state -> Rest.result (singleton $ MimeType.json acceptJson) req state)
  # Rest.yeeha
  where
    init req = Rest.initResult req {load: minLoad}

    malformedRequest req state =
      do
        case method req of
          "POST" -> do
            body <- allBody req mempty
            let
              state2 = hush $ readJSON $ (binaryToString body)
            Rest.result (isNothing state2) req (fromMaybe state state2)
          _ ->
            Rest.result false req state

    provideJson req state =
      do
        currentLoad <- Load.load
        let
          result = {currentLoad}
        Rest.result (JSON.writeJSON result) req state

    acceptJson req state@{load: currentLoad} =
      do
        _ <- Load.setLoad currentLoad
        Rest.result true req state

allBody :: Req -> IOData -> Effect Binary
allBody req acc = do
  readResult <- (readBody req)
  case readResult of
       (FullData body req2) -> pure $ toBinary $ acc <> (fromBinary body)
       (PartialData body req2) -> (allBody req2 $ acc <> (fromBinary body))

binaryToString :: Binary -> String
binaryToString = unsafeCoerce
