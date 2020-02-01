module Rtsv2.Agents.Locator.Types
       ( LocationResp
       , FailureReason(..)
       , LocalOrRemote(..)
       , ResourceResp(..)
       , NoCapacity(..)
       , RegistrationResp(..)
       , ServerSelectionPredicate
       , FindAndRegisterConfig
       , FindOrStartConfig
       )
       where

import Prelude

import Data.Either (Either)
import Data.Maybe (Maybe)
import Effect (Effect)
import Logger (Logger)
import Shared.Types (Server, ServerLoad)



type FindOrStartConfig payload
  = { findFun :: (payload -> Effect (Maybe (LocalOrRemote Server)))
    , handlerCreationPredicate :: ServerSelectionPredicate
    , startLocalFun :: payload -> Effect Unit
    , logWarning :: forall a. Logger a
    }



type FindAndRegisterConfig payload
  = { registerFun :: payload -> Effect Unit
    , findFun :: (payload -> Effect (Maybe (LocalOrRemote Server)))
    , handlerCreationPredicate :: ServerSelectionPredicate
    , startLocalFun :: payload -> Effect Unit
    , startRemoteFun :: ServerLoad -> payload -> Effect Unit
    , logWarning :: forall a. Logger a
    }





type ServerSelectionPredicate = ServerLoad -> Boolean

data FailureReason
  = NotFound
  | NoResource

data LocalOrRemote a
  = Local a
  | Remote a
derive instance functorLocalOrRemoteF :: Functor LocalOrRemote

type LocationResp = (Either FailureReason (LocalOrRemote Server))


--------------------------------------------------------------------------------
-- API Types - maybe move me
--------------------------------------------------------------------------------
type ResourceResp a = Either NoCapacity (LocalOrRemote a)

data NoCapacity = NoCapacity

type RegistrationResp = (Either FailureReason Unit)
