module Rtsv2.Handler.Relay
       ( resource
       , CreateRelayPayload
       ) where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe, fromMaybe')
import Data.Newtype (wrap)
import Effect (Effect)
import Erl.Atom (atom)
import Erl.Cowboy.Req (ReadBodyResult(..), Req, binding, method, readBody)
import Erl.Data.Binary (Binary)
import Erl.Data.Binary.IOData (IOData, fromBinary, toBinary)
import Erl.Data.List (singleton)
import Rtsv2.Agents.IntraPoP as IntraPoP
import Rtsv2.Agents.TransPoP as TransPoP
import Rtsv2.Handler.MimeType as MimeType
import Shared.Stream (StreamId(..))
import Shared.Types (PoPName, ServerAddress)
import Shared.Utils (lazyCrashIfMissing)
import Simple.JSON as JSON
import Stetson (StetsonHandler)
import Stetson.Rest as Rest
import Unsafe.Coerce as Unsafe.Coerce

type CreateRelayPayload
  = { streamSource :: ServerAddress
    , routes :: Array (Array PoPName)
    }

type State
  = { streamId :: StreamId
    , createRelayPayload :: Maybe CreateRelayPayload
    }

resource :: StetsonHandler State
resource =
  Rest.handler init
  # Rest.malformedRequest malformedRequest
  # Rest.contentTypesAccepted (\req state -> Rest.result (singleton $ MimeType.json acceptJson) req state)
  # Rest.contentTypesProvided (\req state -> Rest.result (singleton $ MimeType.json provideJson) req state)
  # Rest.yeeha
  where
    init req =
      let
        streamId = fromMaybe' (lazyCrashIfMissing "stream_id binding missing") $ binding (atom "stream_id") req
      in
        Rest.initResult req
          { streamId: StreamId streamId
          , createRelayPayload: Nothing
          }

    malformedRequest req state = 
      case method req of
        "POST" -> do
          body <- allBody req mempty
          let
            payload = JSON.readJSON $ (binaryToString body) :: Either _ CreateRelayPayload

          case payload of
            Left _ ->
              Rest.result false req state

            Right createRelayPayload ->
              Rest.result true req (state { createRelayPayload = Just createRelayPayload })
        _ ->
          Rest.result false req state
      
    acceptJson req state@{} =
      do
        (Rest.result true req state)

    provideJson req state =
      do
        currentTransPoP <- IntraPoP.currentTransPoPLeader
        intraPoPRelay <- IntraPoP.health
        transPoPRelay <- TransPoP.health
        let
          result = {intraPoPRelay,
                    transPoPRelay,
                    currentTransPoP : fromMaybe (wrap "") currentTransPoP}
        Rest.result (JSON.writeJSON result) req state

allBody :: Req -> IOData -> Effect Binary
allBody req acc = do
  readResult <- (readBody req)
  case readResult of
       (FullData body req2) -> pure $ toBinary $ acc <> (fromBinary body)
       (PartialData body req2) -> (allBody req2 $ acc <> (fromBinary body))

binaryToString :: Binary -> String
binaryToString = Unsafe.Coerce.unsafeCoerce
