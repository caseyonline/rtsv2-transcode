module Rtsv2.Handler.Egest
       ( resource
       ) where

import Prelude

import Data.Either (hush)
import Data.Maybe (Maybe, fromMaybe', isNothing)
import Effect (Effect)
import Erl.Cowboy.Req (ReadBodyResult(..), Req, readBody)
import Erl.Data.Binary (Binary)
import Erl.Data.Binary.IOData (IOData, fromBinary, toBinary)
import Erl.Data.List (singleton, (:))
import Rtsv2.Agents.EgestInstance (CreateEgestPayload)
import Rtsv2.Agents.EgestInstanceSup as EgestInstanceSup
import Rtsv2.Handler.MimeType as MimeType
import Shared.Utils (lazyCrashIfMissing)
import Simple.JSON as JSON
import Stetson (HttpMethod(..), StetsonHandler)
import Stetson.Rest as Rest
import Unsafe.Coerce as Unsafe.Coerce


type State
  = { mPayload :: Maybe CreateEgestPayload
    }

resource :: StetsonHandler State
resource =
  Rest.handler init
  # Rest.allowedMethods (Rest.result (POST : mempty))
  # Rest.malformedRequest malformedRequest
  # Rest.contentTypesAccepted (\req state -> Rest.result (singleton $ MimeType.json acceptJson) req state)
  # Rest.yeeha
  where
    init req =
      do
        mPayload <- hush <$> JSON.readJSON <$> binaryToString <$> allBody req mempty
        Rest.initResult req { mPayload }

    malformedRequest req state@{mPayload} =
      Rest.result (isNothing mPayload) req state

    acceptJson req state@{mPayload} =
      do
        let
          payload = fromMaybe' (lazyCrashIfMissing "impossible noEgestPayload") mPayload
        _ <- EgestInstanceSup.startEgest payload
        Rest.result true req state

allBody :: Req -> IOData -> Effect Binary
allBody req acc = do
  readResult <- (readBody req)
  case readResult of
       (FullData body req2) -> pure $ toBinary $ acc <> (fromBinary body)
       (PartialData body req2) -> (allBody req2 $ acc <> (fromBinary body))

binaryToString :: Binary -> String
binaryToString = Unsafe.Coerce.unsafeCoerce
