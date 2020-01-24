module Rtsv2.Handler.Relay
       ( resource
       , stats
       ) where

import Prelude

import Data.Either (hush)
import Data.Maybe (Maybe(..), fromMaybe', isNothing)
import Data.Newtype (wrap)
import Effect (Effect)
import Erl.Atom (atom)
import Erl.Cowboy.Req (ReadBodyResult(..), Req, StatusCode(..), binding, method, readBody, replyWithoutBody)
import Erl.Data.Binary (Binary)
import Erl.Data.Binary.IOData (IOData, fromBinary, toBinary)
import Erl.Data.List (singleton, (:))
import Erl.Data.Map as Map
import Rtsv2.Agents.StreamRelayInstance (Status, CreateRelayPayload)
import Rtsv2.Agents.StreamRelayInstance as StreamRelayInstance
import Rtsv2.Agents.StreamRelayInstanceSup as StreamRelayInstanceSup
import Rtsv2.Handler.MimeType as MimeType
import Rtsv2.Utils (noprocToMaybe)
import Shared.Stream (StreamId)
import Shared.Utils (lazyCrashIfMissing)
import Simple.JSON as JSON
import Stetson (HttpMethod(..), StetsonHandler)
import Stetson.Rest as Rest
import Unsafe.Coerce as Unsafe.Coerce

type StatsState
  = { streamId :: StreamId
    , mStatus :: Maybe Status
    }

stats :: StetsonHandler StatsState
stats =
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
        mStatus <- noprocToMaybe $ StreamRelayInstance.status streamId
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
        Just status ->
          Rest.result "marvelous" req state

type State
  = { streamId :: StreamId
    , mCreateRelayPayload :: Maybe CreateRelayPayload
    }

resource :: StetsonHandler State
resource =
  Rest.handler init
  # Rest.allowedMethods (Rest.result (POST : mempty))
  # Rest.malformedRequest malformedRequest
  # Rest.contentTypesAccepted (\req state -> Rest.result (singleton $ MimeType.json acceptJson) req state)
  # Rest.contentTypesProvided (\req state -> Rest.result (singleton $ MimeType.json provideContent) req state)
  # Rest.yeeha
  where
    init req =
      let
        streamId = wrap $ fromMaybe' (lazyCrashIfMissing "stream_id binding missing") $ binding (atom "stream_id") req
      in do
        mStatus <- noprocToMaybe $ StreamRelayInstance.status streamId
        mCreateRelayPayload <- hush <$> JSON.readJSON <$> binaryToString <$> allBody req mempty
        Rest.initResult req
            { streamId
            , mCreateRelayPayload
            }

    malformedRequest req state@{streamId, mCreateRelayPayload} =
          Rest.result (isNothing mCreateRelayPayload) req state

    acceptJson req state@{streamId, mCreateRelayPayload} =
      do
        let
          createRelayPayload = fromMaybe' (lazyCrashIfMissing "impossible noPayload") mCreateRelayPayload
        _ <- StreamRelayInstanceSup.startRelay createRelayPayload
        Rest.result true req state

    provideContent req state =
      Rest.result "" req state



allBody :: Req -> IOData -> Effect Binary
allBody req acc = do
  readResult <- (readBody req)
  case readResult of
       (FullData body req2) -> pure $ toBinary $ acc <> (fromBinary body)
       (PartialData body req2) -> (allBody req2 $ acc <> (fromBinary body))

binaryToString :: Binary -> String
binaryToString = Unsafe.Coerce.unsafeCoerce
