module Rtsv2.Handler.LlnwStub
       ( streamAuthType
       , streamAuth
       , streamPublish
       ) where

import Prelude

import Data.Array as Array
import Data.Either (hush)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Maybe (Maybe(..), isNothing)
import Data.Newtype (unwrap, wrap)
import Effect (Effect)
import Erl.Cowboy.Req (ReadBodyResult(..), Req, readBody, setBody)
import Erl.Data.Binary (Binary)
import Erl.Data.Binary.IOData (IOData, fromBinary, toBinary)
import Erl.Data.List (List, filter, head, nil, (:))
import Erl.Data.Tuple (tuple2)
import Logger (spy)
import Rtsv2.Agents.IngestInstanceSup as IngestInstanceSup
import Shared.LlnwApiTypes (AuthType, PublishCredentials, SlotPublishAuthType(..), StreamAuth, StreamConnection, StreamDetails, StreamIngestProtocol(..), StreamPublish)
import Shared.Stream (RtmpShortName, SlotRole(..))
import Simple.JSON (class ReadForeign, class WriteForeign, readJSON, writeJSON)
import Stetson (HttpMethod(..), StetsonHandler)
import Stetson.Rest as Rest
import Unsafe.Coerce (unsafeCoerce)

data StubHost = Any
              | SpecificHost String

derive instance genericStubHost :: Generic StubHost _
instance eqStubHost :: Eq StubHost where eq = genericEq

db :: List { auth :: { host :: StubHost
                     , protocol :: StreamIngestProtocol
                     , rtmpShortName :: RtmpShortName
                     , authType :: SlotPublishAuthType
                     , username :: String
                     , password :: String
                     }
           , details :: StreamDetails
           }
db =
  { auth: { host: Any --"172.16.171.5"
          , protocol: Rtmp
          , rtmpShortName: wrap "mmddev001"
          , authType: Llnw
          , username: "user"
          , password: "password" }
  , details: { role: Primary
             , slot : { id: wrap 1
                      , name: "slot1"
                      , subscribeValidation: false
                      , profiles: [ wrap { name: wrap "high",
                                           rtmpStreamName: wrap "slot1_1000",
                                           bitrate: 1000000}
                                  , wrap { name: wrap "low",
                                           rtmpStreamName: wrap "slot1_500",
                                           bitrate: 500000}
                                  ]
                      , outputFormats : []
                      }
             , push : []
             }
  }
  : nil

streamAuthType :: PostHelper StreamConnection AuthType
streamAuthType =
  postHelper lookup
  where
    lookup :: Maybe StreamConnection -> Maybe AuthType
    lookup Nothing = Nothing
    lookup (Just streamConnectionPayload) =
      let
        {host, protocol, rtmpShortName} = unwrap streamConnectionPayload
      in
        filter (\{auth: { host: candidateHost
                        , protocol: candidateProtocol
                        , rtmpShortName: candidateShortName }} ->
                ((candidateHost == Any) || (candidateHost == SpecificHost host))
                && (candidateProtocol == protocol)
                && (candidateShortName == rtmpShortName))
                db
        # head
        <#> (\x -> {authType: x.auth.authType})

streamAuth :: PostHelper StreamAuth PublishCredentials
streamAuth =
  postHelper lookup
  where
    lookup :: Maybe StreamAuth -> Maybe PublishCredentials
    lookup Nothing = Nothing
    lookup (Just streamAuthPayload) =
      let
        {host, rtmpShortName, username} = unwrap streamAuthPayload
      in
        filter (\{auth: { host: candidateHost
                        , rtmpShortName: candidateShortName
                        , username: candidateUsername }} ->
                ((candidateHost == Any) || (candidateHost == SpecificHost host))
                && (candidateShortName == rtmpShortName)
                && (candidateUsername == username))
                db
        # head
        <#> (\x -> wrap { username: x.auth.username
                        , password: x.auth.password})

streamPublish :: PostHelper StreamPublish StreamDetails
streamPublish =
  postHelper lookup
  where
    lookup :: Maybe StreamPublish -> Maybe StreamDetails
    lookup Nothing = Nothing
    lookup (Just streamPublishPayload) =
      let
        {host, protocol, rtmpShortName, rtmpStreamName, username} = unwrap streamPublishPayload
      in
        filter (\{ auth: { host: candidateHost
                         , protocol: candidateProtocol
                         , rtmpShortName: candidateShortName
                         , username: candidateUsername }
                 , details: { slot: {profiles: candidateProfiles} }} ->
                ((candidateHost == Any) || (candidateHost == SpecificHost host))
                && (candidateProtocol == protocol)
                && (candidateShortName == rtmpShortName)
                && not (Array.null (Array.filter (\candidateProfile ->
                                                    let
                                                       {rtmpStreamName: candidateRtmpStreamName} = unwrap candidateProfile
                                                    in
                                                      candidateRtmpStreamName == rtmpStreamName) candidateProfiles))
                && (candidateUsername == username))
                db
        # head
        <#> _.details

type PostHelper a b = StetsonHandler { payload :: Maybe a
                                     , output :: Maybe b
                                     }
postHelper :: forall a b. ReadForeign a => WriteForeign b =>  (Maybe a -> Maybe b) -> PostHelper a b
postHelper lookupFun =
  Rest.handler (\req -> Rest.initResult req { payload: Nothing
                                            , output: Nothing })

  # Rest.serviceAvailable (\req state -> do
                              isAgentAvailable <- IngestInstanceSup.isAvailable
                              Rest.result isAgentAvailable req state)

  # Rest.allowedMethods (Rest.result (POST : nil))

  # Rest.malformedRequest (\req state -> do
                              body <- allBody req mempty
                              let
                                maybePayload = hush $ readJSON $ binaryToString body
                              Rest.result (isNothing maybePayload) req state{payload = maybePayload})

  # Rest.contentTypesAccepted (\req state ->
                                Rest.result ((tuple2 "application/json" (\req2 state2@{output: maybeOutput} ->
                                                                          let
                                                                            json = case maybeOutput of
                                                                                     Nothing -> ""
                                                                                     Just output -> writeJSON output
                                                                            req3 = setBody json req2
                                                                          in
                                                                          (Rest.result true req3 state2))) : nil)
                                req state)

  -- # Rest.isAuthorized (\req state@{isAuthorized} ->
  --                       Rest.result (Authorized) req state)

  # Rest.resourceExists (\req state@{payload: maybePayload} ->
                          let
                            output = lookupFun maybePayload
                          in
                            case output of
                              Nothing ->
                                Rest.result false req state
                              Just _ ->
                                Rest.result true req state{output = output}
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
