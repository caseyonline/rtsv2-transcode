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
import Erl.Data.List (nil, (:))
import Erl.Data.Tuple (tuple2)
import Rtsv2.Handler.MimeType as MimeType
import Rtsv2.Load as Load
import Simple.JSON (readJSON)
import Simple.JSON as JSON
import Stetson (HttpMethod(..), StetsonHandler)
import Stetson.Rest as Rest
import Unsafe.Coerce (unsafeCoerce)

type State =
  {
    load :: Number
  }

load :: StetsonHandler State
load =
  Rest.handler (\req -> Rest.initResult req {load: 0.0})
  # Rest.allowedMethods (Rest.result (GET : POST : nil))
  # Rest.malformedRequest (\req state ->
                            case method req of
                              "POST" -> do
                                body <- allBody req mempty
                                let
                                  state2 = hush $ readJSON $ (binaryToString body)
                                Rest.result (isNothing state2) req (fromMaybe state state2)
                              _ ->
                                Rest.result false req state
                          )
  # Rest.contentTypesProvided (\req state -> Rest.result (MimeType.json jsonHandler : nil) req state)
  # Rest.contentTypesAccepted (\req state ->
                                Rest.result ((tuple2 "application/json" (\req2 state2@{load: currentLoad} -> do
                                                                            _ <- Load.setLoad currentLoad
                                                                            (Rest.result true req2 state2))) : nil)
                                req state)
  # Rest.yeeha
  where
    jsonHandler req state = do
      currentLoad <- Load.load
      let
        result = {currentLoad}
      Rest.result (JSON.writeJSON result) req state

allBody :: Req -> IOData -> Effect Binary
allBody req acc = do
  readResult <- (readBody req)
  case readResult of
       (FullData body req2) -> pure $ toBinary $ acc <> (fromBinary body)
       (PartialData body req2) -> (allBody req2 $ acc <> (fromBinary body))

binaryToString :: Binary -> String
binaryToString = unsafeCoerce
