module Rtsv2.Agents.Locator
       ( extractServer
       , findAndRegister
       )
       where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Effect (Effect)
import Erl.Utils as Erl
import Logger (spy)
import Rtsv2.Agents.IntraPoP as IntraPoP
import Rtsv2.Agents.Locator.Types (FailureReason(..), FindAndRegisterConfig, LocalOrRemote(..), LocationResp)
import Rtsv2.PoPDefinition as PoPDefinition
import Rtsv2.Utils (noprocToMaybe)


extractServer :: forall a. LocalOrRemote a -> a
extractServer (Local a) = a
extractServer (Remote a) = a


findAndRegister :: forall payload. FindAndRegisterConfig payload ->  payload -> Effect LocationResp
findAndRegister config payload =  do
  -- Try the happy path first and register locally if we are in the correct place
  registerResp <- noprocToMaybe $ config.registerFun payload
  case spy "registerResp" registerResp of
    Just _ -> do
      -- TODO - maybe we should just return Local, not Local Server
      -- or the register function can return server
      thisServer <- PoPDefinition.getThisServer
      pure $ Right $ Local thisServer
    Nothing -> do
      -- Is there already a stream relay in this PoP?
      mExisting <- config.findFun payload
      case mExisting of
        Just (Local server) -> do
          -- Race condition - we could not register, but the infrastructure thinks we are in the right place
          -- wait and try again
          _ <- config.logWarning "Inconsistent location information - retrying" {payload}
          -- TODO sleep duration from config
          _ <- Erl.sleep (wrap 500)
          -- cast the local server to a remote to generate a redirect back here
          pure $ Right $ Remote server
        Just (Remote server) -> do
          pure  $ Right $ Remote server
        Nothing -> do
          newResourceResp <- IntraPoP.getIdleServer config.handlerCreationPredicate
          case newResourceResp of
            Left _ ->
              pure $ Left NoResource
            Right idleServer -> do
              _ <- startLocalOrRemote idleServer
              findAndRegister config payload
  where
    startLocalOrRemote (Local _) = config.startLocalFun payload
    startLocalOrRemote (Remote remote) = config.startRemoteFun remote payload
