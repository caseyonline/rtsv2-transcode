module Rtsv2.LlnwApi
       (
         streamAuthType
       , streamAuth
       , streamPublish
       , slotLookup
       , recordSlotLookup
       , startLink
       , slotLookupCacheUtilization
       )
       where

import Prelude

import Data.Either (Either(..), either)
import Data.Long as Long
import Data.Maybe (maybe)
import Data.Newtype (unwrap, wrap)
import Data.String (Pattern(..), Replacement(..), replace)
import Effect (Effect)
import Ephemeral.Map (EMap)
import Ephemeral.Map as EMap
import Erl.Data.List ((:))
import Erl.Data.Tuple (tuple2)
import Pinto (ServerName, StartLinkResult)
import Pinto.Gen (CallResult(..), CastResult(..))
import Pinto.Gen as Gen
import Pinto.Timer as Timer
import Rtsv2.Agents.IntraPoP as IntraPoP
import Rtsv2.Config (LlnwApiConfig)
import Rtsv2.Config as Config
import Rtsv2.Names as Names
import Shared.Common (Milliseconds, Url(..), CacheUtilization)
import Shared.Rtsv2.LlnwApiTypes (AuthType, PublishCredentials, StreamAuth, StreamConnection, StreamDetails, StreamPublish, SlotLookupResult)
import Shared.Rtsv2.Stream (RtmpShortName, SlotName, rtmpShortNameToString)
import Simple.JSON (class WriteForeign, writeJSON)
import SpudGun (Headers, JsonResponseError, SpudResult, bodyToJSON)
import SpudGun as SpudGun

------------------------------------------------------------------------------
-- API
------------------------------------------------------------------------------
streamAuthType :: LlnwApiConfig -> StreamConnection -> Effect (Either JsonResponseError AuthType)
streamAuthType {streamAuthTypeUrl, headers} body =
  bodyToJSON <$> jsonPost (wrap streamAuthTypeUrl) headers body

streamAuth :: LlnwApiConfig -> StreamAuth -> Effect (Either JsonResponseError PublishCredentials)
streamAuth {streamAuthUrl, headers} body =
  bodyToJSON <$> jsonPost (wrap streamAuthUrl) headers body

streamPublish :: LlnwApiConfig -> StreamPublish -> Effect (Either JsonResponseError StreamDetails)
streamPublish {streamPublishUrl, headers} body =
  bodyToJSON <$> jsonPost (wrap streamPublishUrl) headers body

slotLookup :: LlnwApiConfig -> RtmpShortName -> SlotName -> Effect (Either JsonResponseError SlotLookupResult)
slotLookup {slotLookupUrl, headers} accountName slotName =
  Gen.doCall serverName doSlotLookup
  where
    key = (SlotKey accountName slotName)

    doSlotLookup state@{slotLookupCache} = do
      lookup <- EMap.lookupAndUpdateTime' key slotLookupCache
      maybe (cacheMiss state) (cacheHit state) lookup

    cacheHit state@{slotLookupCacheHits} {value: result, map: newCache} = do
      pure $ CallReply (Right result) state{ slotLookupCache = newCache
                                           , slotLookupCacheHits = slotLookupCacheHits + 1}

    cacheMiss state = do
      let
        url = (replaceAccount >>> replaceSlotName >>> Url) slotLookupUrl
      either (apiFail state) (apiSuccess state) =<< (bodyToJSON <$> jsonGet url headers)

    apiFail state error = do
      pure $ CallReply (Left error) state

    apiSuccess state@{slotLookupCache, slotLookupCacheMisses} slotLookupResult = do
      newCache <- EMap.insert' key slotLookupResult slotLookupCache
      IntraPoP.announceEgestSlotLookup accountName slotName slotLookupResult
      pure $ CallReply (Right slotLookupResult) state{ slotLookupCache = newCache
                                                     , slotLookupCacheMisses = slotLookupCacheMisses + 1}

    replaceAccount = replace (Pattern "{account}") (Replacement $ rtmpShortNameToString accountName)
    replaceSlotName = replace (Pattern "{streamName}") (Replacement $ unwrap slotName)

recordSlotLookup :: RtmpShortName -> SlotName -> SlotLookupResult -> Effect Unit
recordSlotLookup accountName slotName slotLookupResult =
  Gen.doCast serverName doRecordSlotLookup
  where
    key = (SlotKey accountName slotName)

    doRecordSlotLookup state@{slotLookupCache} = do
      newCache <- EMap.insert' key slotLookupResult slotLookupCache
      pure $ CastNoReply state{ slotLookupCache = newCache }

startLink :: Unit -> Effect StartLinkResult
startLink args =
  Gen.startLink serverName init handleInfo

slotLookupCacheUtilization :: Effect CacheUtilization
slotLookupCacheUtilization =
  Gen.call serverName (\state@{ slotLookupCacheHits
                              , slotLookupCacheMisses
                              } -> CallReply { cacheHits: slotLookupCacheHits
                                             , cacheMisses: slotLookupCacheMisses
                                             } state)

------------------------------------------------------------------------------
-- gen_server callbacks
------------------------------------------------------------------------------
init :: Effect State
init = do
  config@{slotLookupExpiryTimeMs} <- Config.llnwApiConfig
  void $ Timer.sendEvery serverName 1000 DoExpiry
  pure { config
       , slotLookupCache: EMap.empty
       , slotLookupCacheHits: 0
       , slotLookupCacheMisses: 0
       , slotLookupExpiryTime: wrap $ Long.fromInt slotLookupExpiryTimeMs
       }

handleInfo :: Msg -> State -> Effect (CastResult State)
handleInfo msg state =
  case msg of
    DoExpiry ->
      CastNoReply <$> doExpiry state

doExpiry :: State -> Effect State
doExpiry state@{slotLookupCache, slotLookupExpiryTime} =
  state{slotLookupCache = _} <$> EMap.garbageCollect' slotLookupExpiryTime slotLookupCache

------------------------------------------------------------------------------
-- Internals
------------------------------------------------------------------------------
data SlotKey = SlotKey RtmpShortName SlotName

type State =
  { config :: Config.LlnwApiConfig
  , slotLookupCache :: EMap SlotKey SlotLookupResult
  , slotLookupExpiryTime :: Milliseconds
  , slotLookupCacheHits :: Int
  , slotLookupCacheMisses :: Int
  }

data Msg = DoExpiry

serverName :: ServerName State Msg
serverName = Names.llnwApiServerName

jsonPost :: forall body. WriteForeign body => Url -> Headers -> body -> Effect SpudResult
jsonPost url headers body =
  SpudGun.post url { headers: ( tuple2 "Content-Type" "application/json" ) : headers
                   , body: writeJSON body
                   }

jsonGet :: Url -> Headers -> Effect SpudResult
jsonGet url headers =
  SpudGun.get url { headers: ( tuple2 "Accept" "application/json" ) : headers
                  }
