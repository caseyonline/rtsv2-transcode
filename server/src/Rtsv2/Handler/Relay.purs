module Rtsv2.Handler.Relay
       ( resource
       , CreateRelayPayload
       ) where

import Prelude

import Data.Maybe (fromMaybe)
import Data.Newtype (wrap)
import Erl.Data.List (nil, singleton, (:))
import Erl.Data.Tuple (tuple2)
import Rtsv2.Agents.IntraPoP as IntraPoP
import Rtsv2.Agents.TransPoP as TransPoP
import Rtsv2.Handler.MimeType as MimeType
import Shared.Stream (StreamId(..))
import Shared.Types (PoPName(..), ServerAddress(..))
import Simple.JSON as JSON
import Stetson (StetsonHandler)
import Stetson.Rest as Rest

type CreateRelayPayload
  = { streamId :: StreamId
    , streamSource :: ServerAddress
    , routes :: Array (Array PoPName)
    }

type State
  = {
    }

resource :: StetsonHandler State
resource =
  Rest.handler (\req -> Rest.initResult req {})
  # Rest.contentTypesAccepted (\req state -> Rest.result (singleton $ MimeType.json acceptJson) req state)
  # Rest.contentTypesProvided (\req state -> Rest.result (singleton $ MimeType.json provideJson) req state)
  # Rest.yeeha
  where
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
