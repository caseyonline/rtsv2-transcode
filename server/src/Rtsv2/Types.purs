module Rtsv2.Types
       (
         LocalOrRemote(..)
       , LocationResp
       , LocalResourceResp(..)
       , LocalResource(..)
       , RegistrationResp(..)
       , ResourceFailed(..)
       , ResourceResp(..)
       , fromLocalOrRemote
       ) where

import Prelude

import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Erl.Process.Raw (Pid)
import Shared.Rtsv2.Types (FailureReason, Server)

data LocalOrRemote a
  = Local a
  | Remote a
derive instance functorLocalOrRemoteF :: Functor LocalOrRemote

type LocationResp = (Either FailureReason (LocalOrRemote Server))

fromLocalOrRemote :: forall a. LocalOrRemote a -> a
fromLocalOrRemote (Local a) = a
fromLocalOrRemote (Remote a) = a

data LocalResource a = LocalResource Pid a
type LocalResourceResp a = Either ResourceFailed (LocalResource a)

type ResourceResp a = Either ResourceFailed (LocalOrRemote a)

data ResourceFailed = NoCapacity
                    | LaunchFailed
                    | InvalidCanaryState
                    | InvalidRunState
                    | AlreadyRunning

type RegistrationResp = (Either FailureReason Unit)

------------------------------------------------------------------------------
-- ResourceFailed
derive instance genericResourceFailed :: Generic ResourceFailed _
instance showResourceFailed :: Show ResourceFailed where show = genericShow
