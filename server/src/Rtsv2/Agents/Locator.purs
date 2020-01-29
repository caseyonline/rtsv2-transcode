module Rtsv2.Agents.Locator
       ( LocationResp
       , FailureReason(..)
       , LocalOrRemote(..)
       , ResourceResponse(..)
       , NoCapacity(..)
       , APIResp(..)
       , extractServer
       )
       where

import Prelude

import Data.Either (Either)
import Shared.Types (Server)


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
type ResourceResponse a = Either NoCapacity (LocalOrRemote a)

data NoCapacity = NoCapacity



extractServer :: forall a. LocalOrRemote a -> a
extractServer (Local a) = a
extractServer (Remote a) = a



type APIResp = (Either FailureReason Unit)
