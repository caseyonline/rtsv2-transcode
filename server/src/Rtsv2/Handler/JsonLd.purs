module Rtsv2.Handler.JsonLd
       (
         getContextJson
       ) where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Erl.Data.List (nil, (:))
import Rtsv2.Handler.MimeType as MimeType
import Shared.Rtsv2.JsonLd as JsonLd
import Shared.Types (JsonLdContextType(..))
import Simple.JSON (writeJSON)
import StetsonHelper (GetHandler, multiMimeResponse)

getContextJson :: JsonLdContextType -> GetHandler (String)
getContextJson contextType =
  multiMimeResponse ((MimeType.json identity) : nil) (Just <$> (doGetContextJson contextType))
  where
    doGetContextJson :: JsonLdContextType -> Effect String
    doGetContextJson ServerContext = pure $ writeJSON JsonLd.serverContext
    doGetContextJson ServerAddressContext = pure $ writeJSON JsonLd.serverAddressContext
    doGetContextJson DeliverToContext = pure $ writeJSON JsonLd.deliverToContext
    doGetContextJson TimedRouteNeighbourContext = pure $ writeJSON JsonLd.timedRouteNeighbourContext
    doGetContextJson ActiveIngestContext = pure $ writeJSON JsonLd.activeIngestContext