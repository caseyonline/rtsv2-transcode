module Rtsv2.Handler.LlnwStub
       ( streamAuthType
       , streamAuth
       , streamPublish
       ) where

import Prelude

import Data.Either (hush)
import Data.Maybe (Maybe(..), isNothing)
import Data.Newtype (wrap)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Erl.Cowboy.Req (ReadBodyResult(..), Req, readBody, setBody)
import Erl.Data.Binary (Binary)
import Erl.Data.Binary.IOData (IOData, fromBinary, toBinary)
import Erl.Data.List (nil, (:))
import Erl.Data.Map (Map, fromFoldable, lookup)
import Erl.Data.Tuple (tuple2)
import Logger (spy)
import Rtsv2.Agents.IngestInstanceSup as IngestInstanceSup
import Shared.LlnwApiTypes (AuthType, PublishCredentials(..), SlotPublishAuthType(..), StreamAuth, StreamDetails, StreamIngestProtocol(..), StreamPublish, StreamConnection)
import Shared.Stream (SlotRole(..))
import Simple.JSON (readJSON, writeJSON)
import Stetson (Authorized(..), HttpMethod(..), StetsonHandler)
import Stetson.Rest as Rest
import Unsafe.Coerce (unsafeCoerce)

streamAuthTypeDb :: Map StreamConnection AuthType
streamAuthTypeDb =
  fromFoldable (Tuple (wrap { host: "172.16.171.5"
                            , protocol: Rtmp
                            , rtmpShortName: "mmddev001"
                            })
                        {authType : Llnw }
                : nil)

streamAuthDb :: Map StreamAuth PublishCredentials
streamAuthDb =
  fromFoldable (Tuple (wrap { host: "172.16.171.5"
                            , rtmpShortName: "mmddev001"
                            , username: "user"
                            })
                (PublishCredentials { username: "user"
                                    , password: "password"})
                : nil)

streamPublishDb :: Map StreamPublish StreamDetails
streamPublishDb =
  fromFoldable (Tuple (wrap { host: "172.16.171.5"
                            , protocol: Rtmp
                            , rtmpShortName: "mmddev001"
                            , rtmpStreamName: "slot1_1000"
                            , username: "user" })
                      { role: Primary
                      , slot : { id: wrap 1
                               , name: "slot1"
                               , subscribeValidation: false
                               , profiles: [ wrap { name: wrap "high",
                                                    rtmpStreamName: "slot1_1000",
                                                    bitrate: 1000000}
                                           , wrap { name: wrap "low",
                                                    rtmpStreamName: "slot1_500",
                                                    bitrate: 500000}
                                           ]
                               , outputFormats : []
                              }
                      , push : []
                      }
                : Tuple (wrap { host: "172.16.171.5"
                              , protocol: Rtmp
                              , rtmpShortName: "mmddev001"
                              , rtmpStreamName: "slot1_500"
                              , username: "user" })
                        { role: Primary
                        , slot : { id: wrap 1
                                 , name: "slot1"
                                 , subscribeValidation: false
                                 , profiles: [ wrap { name: wrap "high",
                                                      rtmpStreamName: "slot1_1000",
                                                      bitrate: 1000000}
                                             , wrap { name: wrap "low",
                                                      rtmpStreamName: "slot1_500",
                                                      bitrate: 500000}
                                             ]
                                 , outputFormats : []
                                 }
                        , push : []
                        }
                : nil )

type StreamAuthTypeState = { streamConnection :: Maybe StreamConnection
                           , authType :: Maybe AuthType
                           , isAuthorized :: Boolean}
streamAuthType :: StetsonHandler StreamAuthTypeState
streamAuthType =
  Rest.handler (\req -> Rest.initResult req { streamConnection:  Nothing
                                            , authType: Nothing
                                            , isAuthorized: true})

  # Rest.serviceAvailable (\req state -> do
                              isAgentAvailable <- IngestInstanceSup.isAvailable
                              Rest.result isAgentAvailable req state)

  # Rest.allowedMethods (Rest.result (POST : nil))

  # Rest.malformedRequest (\req state -> do
                              body <- allBody req mempty
                              let
                                maybeStreamConnection = hush $ readJSON $ binaryToString body
                              Rest.result (isNothing maybeStreamConnection) req state{streamConnection = maybeStreamConnection})

  # Rest.contentTypesAccepted (\req state ->
                                Rest.result ((tuple2 "application/json" (\req2 state2@{authType: maybeAuthType} ->
                                                                          let
                                                                            output = case maybeAuthType of
                                                                              Nothing -> ""
                                                                              Just authType -> writeJSON authType
                                                                            req3 = setBody output req2
                                                                          in
                                                                          (Rest.result true req3 state2))) : nil)
                                req state)

  # Rest.isAuthorized (\req state@{isAuthorized} ->
                        Rest.result (Authorized) req state)

  # Rest.resourceExists (\req state@{streamConnection: maybeKey} ->
                          let
                            lookupAuthType :: StreamConnection -> Maybe AuthType
                            lookupAuthType key = lookup key streamAuthTypeDb
                          in
                            case lookupAuthType =<< maybeKey of
                              Nothing ->
                                Rest.result false req state
                              Just authType ->
                                Rest.result true req state{authType = Just authType}
                        )
  # Rest.previouslyExisted (Rest.result false)

  # Rest.allowMissingPost (Rest.result false)

  # Rest.contentTypesProvided (\req state -> Rest.result (tuple2 "application/json" (Rest.result ""): nil) req state)

  # Rest.yeeha

type StreamAuthState = { streamAuth :: Maybe StreamAuth
                       , publishCredentials :: Maybe PublishCredentials
                       , isAuthorized :: Boolean}
streamAuth :: StetsonHandler StreamAuthState
streamAuth =
  Rest.handler (\req -> Rest.initResult req { streamAuth:  Nothing
                                            , publishCredentials: Nothing
                                            , isAuthorized: true})

  # Rest.serviceAvailable (\req state -> do
                              isAgentAvailable <- IngestInstanceSup.isAvailable
                              Rest.result isAgentAvailable req state)

  # Rest.allowedMethods (Rest.result (POST : nil))

  # Rest.malformedRequest (\req state -> do
                              body <- allBody req mempty
                              let
                                maybeStreamAuth = hush $ readJSON $ binaryToString body
                              Rest.result (isNothing maybeStreamAuth) req state{streamAuth = maybeStreamAuth})

  # Rest.contentTypesAccepted (\req state ->
                                Rest.result ((tuple2 "application/json" (\req2 state2@{publishCredentials: maybePublishCredentials} ->
                                                                          let
                                                                            output = case maybePublishCredentials of
                                                                              Nothing -> ""
                                                                              Just publishCredentials -> writeJSON publishCredentials
                                                                            req3 = setBody output req2
                                                                          in
                                                                          (Rest.result true req3 state2))) : nil)
                                req state)

  # Rest.isAuthorized (\req state@{isAuthorized} ->
                        Rest.result (Authorized) req state)

  # Rest.resourceExists (\req state@{streamAuth: maybeKey} ->
                          let
                            lookupPublishCredentials :: StreamAuth -> Maybe PublishCredentials
                            lookupPublishCredentials key = lookup key streamAuthDb
                          in
                            case lookupPublishCredentials =<< maybeKey of
                              Nothing ->
                                Rest.result false req state
                              Just publishCredentials ->
                                Rest.result true req state{publishCredentials = Just publishCredentials}
                        )
  # Rest.previouslyExisted (Rest.result false)

  # Rest.allowMissingPost (Rest.result false)

  # Rest.contentTypesProvided (\req state -> Rest.result (tuple2 "application/json" (Rest.result ""): nil) req state)

  # Rest.yeeha

type StreamPublishState = { streamPublish :: Maybe StreamPublish
                          , streamDetails :: Maybe StreamDetails
                          , isAuthorized :: Boolean}
streamPublish :: StetsonHandler StreamPublishState
streamPublish =
  Rest.handler (\req -> Rest.initResult req { streamPublish:  Nothing
                                            , streamDetails: Nothing
                                            , isAuthorized: true})

  # Rest.serviceAvailable (\req state -> do
                              isAgentAvailable <- IngestInstanceSup.isAvailable
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
                                                                            req3 = setBody (spy "output" output) req2
                                                                          in
                                                                          (Rest.result true req3 state2))) : nil)
                                req state)

  # Rest.isAuthorized (\req state@{isAuthorized} ->
                        Rest.result (Authorized) req state)

  # Rest.resourceExists (\req state@{streamPublish: maybeKey} ->
                          let
                            lookupStreamDetails :: StreamPublish -> Maybe StreamDetails
                            lookupStreamDetails key = lookup key streamPublishDb
                          in
                            case lookupStreamDetails =<< maybeKey of
                              Nothing ->
                                Rest.result false req state
                              Just streamDetails ->
                                Rest.result true req state{streamDetails = Just streamDetails}
                        )
  # Rest.previouslyExisted (Rest.result false)

  # Rest.allowMissingPost (Rest.result false)

  # Rest.contentTypesProvided (\req state -> Rest.result (tuple2 "application/json" (Rest.result ""): nil) req state)
  --# Rest.preHook (preHookSpyState "LLNW:streamPublish")
  # Rest.yeeha


allBody :: Req -> IOData -> Effect Binary
allBody req acc = do
  readResult <- (readBody req)
  case readResult of
       (FullData body req2) -> pure $ toBinary $ acc <> (fromBinary body)
       (PartialData body req2) -> (allBody req2 $ acc <> (fromBinary body))

binaryToString :: Binary -> String
binaryToString = unsafeCoerce
