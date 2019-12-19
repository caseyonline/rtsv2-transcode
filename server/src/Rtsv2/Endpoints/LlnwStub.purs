module Rtsv2.Endpoints.LlnwStub
       ( streamPublish
       ) where

import Prelude


import Data.Either (hush)
import Data.Maybe (Maybe(..), isNothing)
import Data.Tuple (Tuple(..))
import Debug.Trace (spy)
import Effect (Effect)
import Erl.Cowboy.Req (ReadBodyResult(..), Req, readBody, setBody)
import Erl.Data.Binary (Binary)
import Erl.Data.Binary.IOData (IOData, fromBinary, toBinary)
import Erl.Data.List (nil, (:))
import Erl.Data.Map (Map, fromFoldable, lookup)
import Erl.Data.Tuple (tuple2)
import Rtsv2.IngestAgentInstanceSup as IngestAgentInstanceSup
import Rtsv2.LlnwApiTypes ( SlotPublishAuth(..)
                          , SlotPublishAuthType(..)
                          , StreamDetails
                          , StreamPublish
                          , StreamPublishProtocol(..)
                          , StreamRole(..))
import Simple.JSON (readJSON, writeJSON)
import Stetson (Authorized(..), HttpMethod(..), StetsonHandler)
import Stetson.Rest as Rest
import Unsafe.Coerce (unsafeCoerce)

streamDb :: Map StreamPublish StreamDetails
streamDb =
  fromFoldable (Tuple { host: "172.16.171.5"
                      , protocol: Rtmp
                      , shortname: "mmddev001"
                      , streamName: "my-cool-slot_1000"}
                      { role: Primary
                      , slot : { name: "my-cool-slot"
                                , publishAuth: SlotPublishAuth { authType: Adobe
                                                               , username: "user"
                                                               , password: "password"}
                                , subscribeValidation: false
                                , profiles: ({ name: "100-",
                                               streamName: "my-cool-slot_1000",
                                               bitrate: 1000000} : nil)
                              }
                      , push : nil
                      }
                : nil)

getStream :: StreamPublish -> Maybe StreamDetails
getStream key = lookup key streamDb

type StreamPublishState =
  { streamPublish :: Maybe StreamPublish
  , streamDetails :: Maybe StreamDetails
  , isAuthorized :: Boolean
  }

streamPublish :: StetsonHandler StreamPublishState
streamPublish =
  Rest.handler (\req -> Rest.initResult req { streamPublish:  Nothing
                                            , streamDetails: Nothing
                                            , isAuthorized: true})

  # Rest.serviceAvailable (\req state -> do
                              isAgentAvailable <- IngestAgentInstanceSup.isAvailable
                              Rest.result isAgentAvailable req state)

  # Rest.allowedMethods (Rest.result (POST : nil))

  # Rest.malformedRequest (\req state -> do
                              body <- allBody req mempty
                              let
                                maybeStreamPublish = hush $ readJSON $ binaryToString body
                              Rest.result (isNothing maybeStreamPublish) req state{streamPublish = maybeStreamPublish})

  # Rest.contentTypesAccepted (\req state ->
                                Rest.result ((tuple2 "application/json" (\req2 state2@{streamDetails: maybeStreamDetails} ->
                                                                          let
                                                                            output = case maybeStreamDetails of
                                                                              Nothing -> ""
                                                                              Just streamDetails -> writeJSON streamDetails
                                                                            req3 = setBody output req2
                                                                          in
                                                                          (Rest.result true req3 state2))) : nil)
                                req state)

  # Rest.isAuthorized (\req state@{isAuthorized} ->
                        Rest.result (Authorized) req state)

  # Rest.resourceExists (\req state@{streamPublish: key} ->
                          case getStream =<< key of
                            Nothing ->
                              Rest.result false req state
                            Just streamDetails ->
                              Rest.result true req state{streamDetails = Just streamDetails}
                        )
  # Rest.previouslyExisted (Rest.result false)

  # Rest.allowMissingPost (Rest.result false)

  # Rest.contentTypesProvided (\req state -> Rest.result (tuple2 "application/json" (Rest.result ""): nil) req state)
  
  # Rest.yeeha

allBody :: Req -> IOData -> Effect Binary
allBody req acc = do
  readResult <- (readBody req)
  case readResult of
       (FullData body req2) -> pure $ toBinary $ acc <> (fromBinary body)
       (PartialData body req2) -> (allBody req2 $ acc <> (fromBinary body))

binaryToString :: Binary -> String
binaryToString = unsafeCoerce
