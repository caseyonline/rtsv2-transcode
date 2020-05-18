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
import Logger (Logger)
import Logger as Logger
import Rtsv2.Agents.EgestInstanceSup as EgestInstanceSup
import Rtsv2.Config (LoadConfig)
import Rtsv2.Config as Config
import Rtsv2.LlnwApi as LlnwApi
import Rtsv2.Utils (chainIntoEither)
import Shared.Common (Url)
import Shared.Rtsv2.LlnwApiTypes (SlotLookupResult)
import Shared.Rtsv2.Router.Endpoint (Endpoint(..), makeUrl)
import Shared.Rtsv2.Stream (SlotId, SlotRole(..))
import Shared.Rtsv2.Types (Canary(..), FailureReason, fromLocalOrRemote)
import Stetson (StetsonHandler)
import StetsonHelper (jsonResponse)

discover :: LoadConfig -> Canary -> String -> String -> StetsonHandler (Maybe (List Url))
discover loadConfig canary accountName streamName =
  jsonResponse getUrls

  where
    getUrls =
      do
        config <- Config.llnwApiConfig
        result <- (LlnwApi.slotLookup config accountName streamName) >>= chainIntoEither getSessionUrls

        case result of
          Left lookupError ->
            do
              _ <- logWarning "Slot lookup failed during stream discovery" { accountName, streamName, lookupError }
              pure Nothing

          Right { urls, slotId, errors } | urls == nil->
            do
              _ <- logWarning "No playback URLs were obtainable for slot" { accountName, streamName, slotId, errors }
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
    getSessionUrl role slot =
      let
        endpoint = case canary of
                     Live -> ClientPlayerControlE slot.id role
                     Canary -> CanaryClientPlayerControlE slot.id role
      in
        EgestInstanceSup.findEgest loadConfig canary slot.id role <#> map ( fromLocalOrRemote >>> ((flip makeUrl) endpoint))


--------------------------------------------------------------------------------
-- Log helpers
--------------------------------------------------------------------------------
domains :: List Atom
domains = atom "StreamDiscovery" # singleton

logInfo :: forall a. Logger (Record a)
logInfo = domainLog Logger.info

logWarning :: forall a. Logger (Record a)
logWarning = domainLog Logger.warning

domainLog :: forall a. Logger {domain :: List Atom, misc :: Record a} -> Logger (Record a)
domainLog = Logger.doLog domains
