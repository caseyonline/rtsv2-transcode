module Rtsv2App.AppM where

import Prelude

import Control.Monad.Reader.Trans (class MonadAsk, ReaderT, ask, asks, runReaderT)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Aff.Bus as Bus
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console as Console
import Effect.Now as Now
import Effect.Ref as Ref
import Record.Extra (sequenceRecord)
import Routing.Duplex (print)
import Routing.Hash (setHash)
import Rtsv2App.Api.Endpoint.Auth as Auth
import Rtsv2App.Api.Request (RequestMethod(..))
import Rtsv2App.Api.Request as Request
import Rtsv2App.Api.Utils (authenticate, mkAuthRequest, mkOriginRequest, mkRequest)
import Rtsv2App.Capability.LogMessages (class LogMessages)
import Rtsv2App.Capability.Navigate (class Navigate, navigate)
import Rtsv2App.Capability.Now (class Now)
import Rtsv2App.Capability.Resource.Api (class ManageApi)
import Rtsv2App.Capability.Resource.User (class ManageUser)
import Rtsv2App.Data.Log as Log
import Rtsv2App.Data.Profile (ProfileEmailRes)
import Rtsv2App.Data.Route as Route
import Rtsv2App.Env (Env, LogLevel(..))
import Shared.Rtsv2.Agent.State (IntraPoP, PoPDefinition, TimedPoPRoutes, IngestAggregator)
import Shared.Rtsv2.Router.Endpoint.Public as Public
import Shared.Rtsv2.Router.Endpoint.Support as Support
import Simple.JSON as JSON
import Type.Equality (class TypeEquals, from)


-- | `AppM` combines the `Aff` and `Reader` monads under a new type, which we can now use to write
-- | instances for our capabilities. We're able to combine these monads because `ReaderT` is a
-- | monad transformer.
newtype AppM a = AppM (ReaderT Env Aff a)

-- | Use `runReaderT` along with the environment to supply throughout the application to
-- | eliminate `Reader` altogether and get left with only `Aff`.
runAppM :: Env -> AppM ~> Aff
runAppM env (AppM m) = runReaderT m env

-- | get a monad out of our `AppM` type essentially for free by deferring to the underlying
-- | `ReaderT` instances.
derive newtype instance functorAppM :: Functor AppM
derive newtype instance applyAppM :: Apply AppM
derive newtype instance applicativeAppM :: Applicative AppM
derive newtype instance bindAppM :: Bind AppM
derive newtype instance monadAppM :: Monad AppM
derive newtype instance monadEffectAppM :: MonadEffect AppM
derive newtype instance monadAffAppM :: MonadAff AppM

instance monadAskAppM :: TypeEquals e Env => MonadAsk e AppM where
  ask = AppM $ asks from

-- | Retrieve the current time.
instance nowAppM :: Now AppM where
  now = liftEffect Now.now
  nowDate = liftEffect Now.nowDate
  nowTime = liftEffect Now.nowTime
  nowDateTime = liftEffect Now.nowDateTime

-- | Use global environment to decide whether to log all messages
-- | (`Dev`) or just important messages (`Prod`).
instance logMessagesAppM :: LogMessages AppM where
  logMessage log = do
    env <- ask
    liftEffect case env.logLevel, Log.reason log of
      Prod, Log.Debug -> pure unit
      _, _ -> Console.log $ Log.message log

-- | Hash-based routing and logging out it's value removing tokens
instance navigateAppM :: Navigate AppM where
  navigate =
    liftEffect <<< setHash <<< print Route.routeCodec

  logout = do
    { currentUser, userBus } <- asks _.userEnv
    liftEffect do
      Ref.write Nothing currentUser
      Request.removeToken
    liftAff do
      Bus.write Nothing userBus
    navigate Route.DashboardR

-- | all of the available user requests
instance manageUserAppM :: ManageUser AppM where
  loginUser =
    authenticate Request.login

  registerUser =
    authenticate Request.register

  getCurrentUser = do
    response <- mkAuthRequest { endpoint: Auth.UserE, method: Get }
    case response of
      Left e -> pure Nothing
      Right res ->
        case JSON.readJSON res of
          Left e -> pure Nothing
          Right (r :: ProfileEmailRes) -> do
            pure $ Just r.user

  updateUser fields =
    void $ mkAuthRequest { endpoint: Auth.UserE, method: Put (Just (JSON.writeJSON fields)) }

-- | all api stats related requests
instance manageAPIAppM :: ManageApi AppM where
  -- | get round trip routes taken given from popname to serverId
  getTimedRoutes sInfo curPopName = do
    case sequenceRecord sInfo of
      Nothing -> pure $ Left "SPoPInfo not present"
      Just s@{ selectedSlotId, selectedAddress } -> do
        response <- mkOriginRequest selectedAddress { endpoint: Support.TimedRoutesForPoPE curPopName, method: Get }
        case response of
          Left e -> pure $ Left e
          Right res ->
            case JSON.readJSON res of
              Left e -> pure $ Left $ show e
              Right (r :: (TimedPoPRoutes Array)) -> do
                pure $ Right r

  getAggregatorDetails { slotId, slotRole, serverAddress } = do
    response <- mkOriginRequest serverAddress { endpoint: Support.IngestAggregatorE slotId slotRole, method: Get }
    case response of
          Left e -> pure $ Left e
          Right res ->
            case JSON.readJSON res of
              Left e -> pure $ Left $ res
              Right (r :: (IngestAggregator Array)) -> do
                pure $ Right r

  getPoPdefinition = do
    response <- mkRequest { endpoint: Support.PoPDefinitionE, method: Get }
    case response of
          Left e -> pure $ Left e
          Right res ->
            case JSON.readJSON res of
              Left e -> pure $ Left $ show e
              Right (r :: PoPDefinition Array) -> do
                pure $ Right r

  getServerState serverAddress = do
    case serverAddress of
      Nothing -> pure $ Left "No server address given"
      Just sa -> do
        response <- mkOriginRequest sa { endpoint: Support.ServerStateE, method: Get }
        case response of
          Left e -> pure $ Left e
          Right res ->
            case JSON.readJSON res of
              Left e -> pure $ Left $ show e
              Right (r :: IntraPoP Array) -> do
                pure $ Right r
