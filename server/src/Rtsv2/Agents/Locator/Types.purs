module Rtsv2.Agents.Locator.Types
       ( -- LocationResp
       -- , FailureReason(..)
       -- , LocalOrRemote(..)
       -- , ResourceResp(..)
       -- , ResourceFailed(..)
       -- , RegistrationResp(..)
          FindAndRegisterConfig
       , FindOrStartConfig
--       , fromLocalOrRemote
       )
       where

import Prelude

import Data.Either (Either)
import Data.Maybe (Maybe)
import Effect (Effect)
import Logger (Logger)
import Rtsv2.LoadTypes (ServerSelectionPredicate)
import Shared.Rtsv2.Types (LocalOrRemote, Server)

type FindOrStartConfig
  = { findFun :: Effect (Maybe (LocalOrRemote Server))
    , handlerCreationPredicate :: ServerSelectionPredicate
    , startLocalFun :: Effect Unit
    , logWarning :: forall a. Logger (Record a)
    }

type FindAndRegisterConfig payload
  = { registerFun :: payload -> Effect Unit
    , findFun :: (payload -> Effect (Maybe (LocalOrRemote Server)))
    , handlerCreationPredicate :: ServerSelectionPredicate
    , startLocalFun :: payload -> Effect Unit
    , startRemoteFun :: Server -> payload -> Effect Unit
    , logWarning :: forall a. Logger (Record a)
    }
