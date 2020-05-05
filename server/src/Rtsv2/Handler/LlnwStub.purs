module Rtsv2.Handler.LlnwStub
       ( streamAuthType
       , streamAuth
       , streamPublish
       , slotLookup
       , hlsPush
       , db
       , StubHost
       , StubProtocol
       ) where

import Prelude
import Shared.Rtsv2.LlnwApiTypes

import Data.Array as Array
import Data.Either (either, hush)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Maybe (Maybe(..), fromMaybe, fromMaybe', isNothing, isJust, fromJust)
import Data.Newtype (unwrap, wrap)
import Data.String (joinWith)
import Effect (Effect)
import Effect.Exception (throw)
import Erl.Atom (Atom, atom)
import Erl.Cowboy.Req (ReadBodyResult(..), Req, readBody, setBody)
import Erl.Data.Binary (Binary)
import Erl.Data.Binary.IOData (IOData, fromBinary, toBinary)
import Erl.Data.Binary.IOData as IOData
import Erl.Data.List (List, filter, head, nil, (:))
import Erl.Data.Tuple (tuple2)
import Erl.File as File
import Erl.FileLib as FileLib
import Logger (Logger)
import Logger as Logger
import Partial.Unsafe (unsafePartial)
import Rtsv2.Agents.IngestSup as IngestSup
import Rtsv2.Handler.MimeType as MimeType
import Shared.Rtsv2.Stream (RtmpShortName, SlotRole(..))
import Shared.UUID (fromString)
import Shared.Utils (lazyCrashIfMissing)
import Simple.JSON (class ReadForeign, class WriteForeign, readJSON, writeJSON)
import Stetson (Authorized(..), HttpMethod(..), StetsonHandler)
import Stetson.Rest as Rest
import Unsafe.Coerce (unsafeCoerce)


data StubHost = AnyHost
              | SpecificHost String

derive instance genericStubHost :: Generic StubHost _
instance eqStubHost :: Eq StubHost where eq = genericEq

data StubProtocol = AnyProtocol
                  | SpecificProtocol StreamIngestProtocol

derive instance genericStubProtocol :: Generic StubProtocol _
instance eqStubProtocol :: Eq StubProtocol where eq = genericEq

type SlotDbEntry =
  { auth :: { host :: StubHost
            , protocol :: StubProtocol
            , rtmpShortName :: RtmpShortName
            , authType :: SlotPublishAuthType
            , username :: String
            , password :: String
            }
  , details :: StreamDetails
  }

db :: List SlotDbEntry
db =
  slotA : slotB : webRtcSlotA : nil

  where
    slotA =
      { auth: { host: AnyHost --"172.16.171.5"
              , protocol: AnyProtocol
              , rtmpShortName: wrap "mmddev001"
              , authType: Adobe
              , username: "user"
              , password: "password" }
      , details: { role: Primary
                 , slot : { id: wrap $ fromMaybe' (lazyCrashIfMissing "Invalid UUID") (fromString "00000000-0000-0000-0000-000000000001")
                          , name: "slot1"
                          , subscribeValidation: false
                          , profiles: [ wrap { name: wrap "high",
                                               rtmpStreamName: wrap "slot1_1000",
                                               bitrate: 1000000}
                                      , wrap { name: wrap "low",
                                               rtmpStreamName: wrap "slot1_500",
                                               bitrate: 500000}
                                      ]
                          , outputFormats : [ HlsOutput ]
                          }
                 , push : [
                    { protocol: HttpPut
                    , formats: [ Hls ]
                    , putBaseUrl: "http://172.16.171.1:3000/system/llnwstub/rts/v1/hls/test_slot_1/"
                    , playbackBaseUrl: "" -- ie relative
                    , segmentDuration: 3
                    , playlistDuration: 20
                    , auth:
                      { type: "basic"
                      , username: "id3as"
                      , password: "id3as"
                      }
                    }
                  ]
                 }
      }

    slotB =
      { auth: { host: AnyHost --"172.16.171.5"
              , protocol: SpecificProtocol Rtmp
              , rtmpShortName: wrap "mmddev001"
              , authType: Llnw
              , username: "user"
              , password: "password" }
      , details: { role: Backup
                 , slot : { id: wrap $ fromMaybe' (lazyCrashIfMissing "Invalid UUID") (fromString "00000000-0000-0000-0000-000000000001")
                          , name: "slot1b"
                          , subscribeValidation: false
                          , profiles: [ wrap { name: wrap "high",
                                               rtmpStreamName: wrap "slot1b_1000",
                                               bitrate: 1000000}
                                      , wrap { name: wrap "low",
                                               rtmpStreamName: wrap "slot1b_500",
                                               bitrate: 500000}
                                      ]
                          , outputFormats : []
                          }
                 , push : []
                 }
      }

    webRtcSlotA =
      { auth: { host: AnyHost --"172.16.171.5"
              , protocol: SpecificProtocol WebRTC
              , rtmpShortName: wrap "mmddev002"
              , authType: Adobe
              , username: "user"
              , password: "password" }
      , details: { role: Primary
                 , slot : { id: wrap $ fromMaybe' (lazyCrashIfMissing "Invalid UUID") (fromString "00000000-0000-0000-0000-000000000002")
                          , name: "slot2"
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

streamAuthType :: PostHelper StreamConnection AuthType
streamAuthType =
  postHelper lookup
  where
    lookup = lookup' <<< (map unwrap)

    lookup' Nothing = Nothing
    lookup' (Just {host, protocol, rtmpShortName}) =
      filter (\ {auth: { host: candidateHost
                       , protocol: candidateProtocol
                       , rtmpShortName: candidateShortName }} ->
              ((candidateHost == AnyHost) || (candidateHost == SpecificHost host))
              && ((candidateProtocol == AnyProtocol) || (candidateProtocol == SpecificProtocol protocol))
              && (candidateShortName == rtmpShortName))
      db
      # head
      <#> (\x -> {authType: x.auth.authType})

streamAuth :: PostHelper StreamAuth PublishCredentials
streamAuth =
  postHelper lookup
  where
    lookup = lookup' <<< (map unwrap)

    lookup' Nothing = Nothing
    lookup' (Just {host, rtmpShortName, username}) =
      filter (\ {auth: { host: candidateHost
                      , rtmpShortName: candidateShortName
                      , username: candidateUsername }} ->
              ((candidateHost == AnyHost) || (candidateHost == SpecificHost host))
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
    lookup = lookup' <<< (map unwrap)

    lookup' Nothing = Nothing
    lookup' (Just {host, protocol, rtmpShortName, rtmpStreamName, username}) =
      filter (\ { auth: { host: candidateHost
                       , protocol: candidateProtocol
                       , rtmpShortName: candidateShortName
                       , username: candidateUsername }
               , details: { slot: {profiles: candidateProfiles} }} ->
              ((candidateHost == AnyHost) || (candidateHost == SpecificHost host))
              && ((candidateProtocol == AnyProtocol) || (candidateProtocol == SpecificProtocol protocol))
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


slotLookup :: String -> String -> StetsonHandler Unit
slotLookup accountName streamName =
  Rest.handler (\req -> Rest.initResult req unit)
  # Rest.resourceExists (\req state -> Rest.result (isJust matchingEntry) req state)
  # Rest.contentTypesProvided (\req state -> Rest.result (MimeType.json jsonHandler : nil) req unit)
  # Rest.yeeha
  where
    jsonHandler req state =
      let
        justEntry = unsafePartial $ fromJust matchingEntry
        result =
          { id: justEntry.details.slot.id
          } :: SlotLookupResult
      in
        Rest.result (writeJSON result) req state

    matchingEntry =
      filter isMatchingEntry db # head

    isMatchingEntry entry =
      (unwrap entry.auth.rtmpShortName) == accountName
      && entry.details.slot.name == streamName

type PostHelper a b = StetsonHandler { payload :: Maybe a
                                     , output :: Maybe b
                                     }
postHelper :: forall a b. ReadForeign a => WriteForeign b =>  (Maybe a -> Maybe b) -> PostHelper a b
postHelper lookupFun =
  Rest.handler (\req ->
                 Rest.initResult req { payload: Nothing
                                     , output: Nothing })

  # Rest.serviceAvailable (\req state -> do
                              isAgentAvailable <- IngestSup.isAgentAvailable
                              Rest.result isAgentAvailable req state)

  # Rest.allowedMethods (Rest.result (POST : nil))

  # Rest.malformedRequest (\req state -> do
                              body <- allBody req mempty
                              let
                                eJSON = readJSON $ binaryToString body
                                maybePayload = hush $ eJSON
                              Rest.result (isNothing maybePayload) req state{payload = maybePayload})

  # Rest.contentTypesAccepted (\req state ->
                                Rest.result ((tuple2 "application/json" (\req2 state2@{output: maybeOutput} ->
                                                                          let
                                                                            json = fromMaybe "" $ writeJSON <$> maybeOutput
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
  --# Rest.preHook (preHookSpyState "LLNW:postHelper")
  # Rest.yeeha

foreign import isAuthorized :: Req -> Boolean

hlsPush :: Array String -> StetsonHandler Unit
hlsPush path =
  Rest.handler (\req -> Rest.initResult req unit)
  # Rest.allowedMethods (Rest.result (PUT : nil))
  # Rest.contentTypesProvided (\req state -> Rest.result (MimeType.any anyHandler : nil) req unit)
  # Rest.contentTypesAccepted (\req state -> Rest.result (tuple2 "application/octet-stream" acceptPut : nil) req unit)
  # Rest.isAuthorized isAuth
  # Rest.yeeha

  where
  anyHandler req2 state2 = throw "this is not called"
  acceptPut req state = do
    body <- allBody req mempty
    let fname = "/tmp/rtsv2_hls/" <> joinWith "/" path
    _ <- FileLib.ensureDir fname
    File.writeFile fname (IOData.fromBinary body) >>=
      either (const $ logInfo "Failed to write" {path : joinWith "/" path}) (const $ pure unit)
    Rest.result true req state

  isAuth req state =
    let reply = case isAuthorized req of
                  true -> Authorized
                  false -> NotAuthorized "Basic"
    in
    Rest.result reply req state



allBody :: Req -> IOData -> Effect Binary
allBody req acc = do
  readResult <- (readBody req)
  case readResult of
       (FullData body req2) -> pure $ toBinary $ acc <> (fromBinary body)
       (PartialData body req2) -> (allBody req2 $ acc <> (fromBinary body))

binaryToString :: Binary -> String
binaryToString = unsafeCoerce

domain :: List Atom
domain = atom <$> ("LlnwStub" : nil)

logInfo :: forall a. Logger (Record a)
logInfo = Logger.doLog domain Logger.info
