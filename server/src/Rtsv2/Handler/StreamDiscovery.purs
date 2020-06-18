module Rtsv2.Handler.StreamDiscovery
       (
         discover
       ) where

import Prelude

import Data.Bifunctor (lmap)
import Data.Either (Either(..), either)
import Data.Filterable (filterMap)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Effect (Effect)
import Erl.Atom (Atom, atom)
import Erl.Data.List (List, nil, (:), singleton)
import Logger as Logger
import Rtsv2.Agents.EgestInstanceSup as EgestInstanceSup
import Rtsv2.Config (LoadConfig)
import Rtsv2.Config as Config
import Rtsv2.LlnwApi as LlnwApi
import Rtsv2.Types (fromLocalOrRemote)
import Rtsv2.Utils (chainIntoEither)
import Shared.Common (Url)
import Shared.Rtsv2.LlnwApiTypes (SlotLookupResult)
import Shared.Rtsv2.Router.Endpoint.Public as Public
import Shared.Rtsv2.Router.Endpoint.Support as Support
import Shared.Rtsv2.Stream (RtmpShortName, SlotId, SlotName, SlotRole(..))
import Shared.Rtsv2.Types (CanaryState(..), FailureReason, Server)
import Stetson (StetsonHandler)
import StetsonHelper (jsonResponse)

discover :: LoadConfig -> CanaryState -> RtmpShortName -> SlotName -> StetsonHandler (Maybe (List Url))
discover loadConfig canary accountName slotName =
  jsonResponse getUrls

  where
    getUrls =
      do
        config <- Config.llnwApiConfig
        result <- (LlnwApi.slotLookup config accountName slotName) >>= chainIntoEither getSessionUrls

        case result of
          Left lookupError ->
            do
              logWarning "Slot lookup failed during stream discovery" { accountName, slotName, lookupError }
              pure Nothing

          Right { urls, slotId, errors } | urls == nil->
            do
              logWarning "No playback URLs were obtainable for slot" { accountName, slotName, slotId, errors }
              pure Nothing

          Right { urls } ->
            -- TODO: log errors here?
            pure $ Just urls


    getSessionUrls :: SlotLookupResult -> Effect { slotId :: SlotId, urls :: List Url, errors :: List { error :: FailureReason, role :: SlotRole } }
    getSessionUrls slot =
      let
        roles = Primary : Backup : nil

        getSessionUrlAndRole slot role =
          getSessionUrl role slot <#> lmap (\error -> { error, role })

      in
        do
          results <- traverse (getSessionUrlAndRole slot) roles

          let
            urls = results # filterMap (either (const Nothing) Just)
            errors = results # filterMap (either Just (const Nothing))

          pure { slotId: slot.id, errors, urls }

    getSessionUrl :: SlotRole -> SlotLookupResult -> Effect (Either FailureReason Url)
    getSessionUrl role slot = do
      locationResp <- EgestInstanceSup.findEgest loadConfig canary slot.id role
      traverse (url <<< fromLocalOrRemote) locationResp

      where
        url :: Server -> Effect Url
        url = case canary of
          Live -> flip Public.makeWsUrl $ Public.ClientPlayerControlE slot.id role
          Canary -> flip Support.makeWsUrl $ Support.CanaryClientPlayerControlE slot.id role

--------------------------------------------------------------------------------
-- Log helpers
--------------------------------------------------------------------------------
domain :: List Atom
domain = atom "StreamDiscovery" # singleton

logInfo :: forall report. String -> { | report } -> Effect Unit
logInfo = Logger.info <<< Logger.traceMetadata domain

logWarning :: forall report. String -> { | report } -> Effect Unit
logWarning = Logger.warning <<< Logger.traceMetadata domain
