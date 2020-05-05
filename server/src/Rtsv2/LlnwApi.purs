module Rtsv2.LlnwApi
       (
         streamAuthType
       , streamAuth
       , streamPublish
       , slotLookup
       )
       where

import Data.Either (Either)
import Data.Newtype (wrap)
import Data.String (Pattern(..), Replacement(..), replace)
import Effect (Effect)
import Erl.Data.List ((:))
import Erl.Data.Tuple (tuple2)
import Prelude ((<#>), (<$>), (>>>))
import Rtsv2.Config (LlnwApiConfig)
import Shared.Common (Url(..))
import Shared.Rtsv2.LlnwApiTypes (AuthType, PublishCredentials, SlotLookupResult, StreamAuth, StreamConnection, StreamDetails, StreamPublish)
import Simple.JSON (class WriteForeign, writeJSON)
import SpudGun (Headers, JsonResponseError, SpudResult, bodyToJSON)
import SpudGun as SpudGun

streamAuthType :: LlnwApiConfig -> StreamConnection -> Effect (Either JsonResponseError AuthType)
streamAuthType {streamAuthTypeUrl, headers} body =
  bodyToJSON <$> jsonPost (wrap streamAuthTypeUrl) headers body

streamAuth :: LlnwApiConfig -> StreamAuth -> Effect (Either JsonResponseError PublishCredentials)
streamAuth {streamAuthUrl, headers} body =
  bodyToJSON <$> jsonPost (wrap streamAuthUrl) headers body

streamPublish :: LlnwApiConfig -> StreamPublish -> Effect (Either JsonResponseError StreamDetails)
streamPublish {streamPublishUrl, headers} body =
  bodyToJSON <$> jsonPost (wrap streamPublishUrl) headers body

slotLookup :: LlnwApiConfig -> String -> String -> Effect (Either JsonResponseError SlotLookupResult)
slotLookup {slotLookupUrl, headers} accountName streamName = do
  let
    url = (replaceAccount >>> replaceStreamName >>> Url) slotLookupUrl
  jsonGet url headers <#> bodyToJSON
  where
    replaceAccount = replace (Pattern "{account}") (Replacement accountName)
    replaceStreamName = replace (Pattern "{streamName}") (Replacement streamName)


jsonPost :: forall body. WriteForeign body => Url -> Headers -> body -> Effect SpudResult
jsonPost url headers body =
  SpudGun.post url { headers: ( tuple2 "Content-Type" "application/json" ) : headers
                   , body: writeJSON body
                   }

jsonGet :: Url -> Headers -> Effect SpudResult
jsonGet url headers =
  SpudGun.get url { headers: ( tuple2 "Accept" "application/json" ) : headers
                  }
