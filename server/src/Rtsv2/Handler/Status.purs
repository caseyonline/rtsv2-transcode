module Rtsv2.Handler.Status
       ( status
       , StatusState
       ) where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe')
import Data.Newtype (wrap)
import Effect (Effect)
import Erl.Atom (atom)
import Erl.Cowboy.Req (StatusCode(..), binding, method, replyWithoutBody)
import Erl.Data.List (singleton, (:))
import Erl.Data.Map as Map
import Rtsv2.Handler.MimeType as MimeType
import Rtsv2.Utils (noprocToMaybe)
import Shared.Stream (StreamId)
import Shared.Utils (lazyCrashIfMissing)
import Simple.JSON (class WriteForeign, writeJSON)
import Stetson (HttpMethod(..), StetsonHandler)
import Stetson.Rest as Rest

type StatusState a
  = { streamId :: StreamId
    , mStatus :: Maybe a
    }

status :: forall a. WriteForeign a =>  (StreamId -> Effect a) -> StetsonHandler (StatusState a)
status statusMethod =
  Rest.handler init
  # Rest.allowedMethods (Rest.result (GET : mempty))
  # Rest.contentTypesProvided (\req state -> Rest.result (singleton $ MimeType.json provideContent) req state)
  # Rest.yeeha
  where
    init req =
      let
        streamId = wrap $ fromMaybe' (lazyCrashIfMissing "stream_id binding missing") $ binding (atom "stream_id") req
        method' = method req
      in do
        mStatus <- noprocToMaybe $ statusMethod streamId
        Rest.initResult req
            { streamId
            , mStatus
            }

    provideContent req state@{mStatus} =
      case mStatus of
        Nothing ->
          do
            newReq <- replyWithoutBody (StatusCode 404) Map.empty req
            Rest.stop newReq state
        Just theStatus ->
          Rest.result (writeJSON theStatus) req state
