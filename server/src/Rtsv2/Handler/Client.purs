module Rtsv2.Handler.Client
       ( clientStart
       , clientStop
       , ClientStartState
       ) where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (fromMaybe')
import Data.Newtype (unwrap, wrap)
import Data.Traversable (sequence, traverse)
import Effect (Effect)
import Erl.Atom (Atom, atom)
import Erl.Cowboy.Handlers.Rest (MovedResult, moved, notMoved)
import Erl.Cowboy.Req (Req, StatusCode(..), binding, replyWithoutBody, setHeader)
import Erl.Data.List (List, singleton, (:))
import Erl.Data.List as List
import Erl.Data.Map as Map
import Erl.Data.Tuple (tuple2)
import Erl.Process.Raw (Pid)
import Erl.Process.Raw as Raw
import Erl.Utils as Erl
import Erl.Utils as Timer
import Gproc as GProc
import Gproc as Gproc
import Logger (Logger, spy)
import Logger as Logger
import Rtsv2.Agents.EgestInstance as EgestInstance
import Rtsv2.Agents.EgestInstanceSup as EgestInstanceSup
import Rtsv2.Agents.Locator.Egest (findEgest)
import Rtsv2.Agents.Locator.Types (FailureReason(..), LocalOrRemote(..))
import Rtsv2.Audit as Audit
import Rtsv2.Handler.MimeType as MimeType
import Rtsv2.PoPDefinition as PoPDefinition
import Rtsv2.Router.Endpoint (Endpoint(..), makeUrl)
import Rtsv2.Web.Bindings as Bindings
import Shared.Stream (EgestKey(..), SlotId)
import Shared.Types (Server, extractAddress)
import Shared.Utils (lazyCrashIfMissing)
import Stetson (HttpMethod(..), RestResult, StetsonHandler)
import Stetson.Rest as Rest


newtype ClientStartState = ClientStartState { slotId :: SlotId
                                            , clientId :: String
                                            , egestResp :: (Either FailureReason (LocalOrRemote Server))
                                            }

clientStart :: StetsonHandler ClientStartState
clientStart =
  Rest.handler init
  # Rest.allowedMethods (Rest.result (POST : mempty))
  # Rest.contentTypesAccepted (\req state -> Rest.result (singleton $ MimeType.json acceptAny) req state)
  # Rest.resourceExists resourceExists
  # Rest.previouslyExisted previouslyExisted
  # Rest.movedTemporarily movedTemporarily
  # Rest.yeeha

  where
    init req = do
      let
        slotId = Bindings.slotId req
        clientId = fromMaybe' (lazyCrashIfMissing $ "client_id binding missing") $ binding (atom "client_id") req
      thisServer <- PoPDefinition.getThisServer
      egestResp <- findEgest slotId thisServer
      let
        req2 = setHeader "x-servedby" (unwrap $ extractAddress thisServer) req
        _ = spy "egestResp" egestResp
      Rest.initResult req2 $ ClientStartState { slotId, clientId, egestResp }

    acceptAny req state = Rest.result true req state

    resourceExists req state@(ClientStartState {slotId, egestResp, clientId}) =
      case egestResp of
        Left NotFound ->
          do
            newReq <- replyWithoutBody (StatusCode 404) Map.empty req
            Rest.stop newReq state
        Left NoResource -> do
          --TODO - don't think this should be a 502
          newReq <- replyWithoutBody (StatusCode 502) Map.empty req
          Rest.stop newReq state
        Right (Local _)  -> do
          handlerPid <- startHandler clientId
          _ <- EgestInstance.addClient handlerPid (EgestKey slotId)
          Rest.result true req state
        Right (Remote _) -> do
          Rest.result false req state

    previouslyExisted req state@(ClientStartState {egestResp}) =
      let resp = case egestResp of
            Right (Remote _) -> true
            _ -> false
      in
       Rest.result resp req state

    movedTemporarily :: Req -> ClientStartState -> Effect (RestResult MovedResult ClientStartState)
    movedTemporarily req state@(ClientStartState {slotId, egestResp}) =
      case spy "moved" egestResp of
        Right (Remote server) ->
          let
            url = makeUrl server (ClientStartE "canary" slotId)
            -- path = Routing.printUrl RoutingEndpoint.endpoint
            -- url = spy "url" $ "http://" <> unwrap addr <> ":3000" <> path
          in
            Rest.result (moved $ unwrap url) req state
        _ ->
          Rest.result notMoved req state





type ClientStopState = { egestKey :: EgestKey
                       , clientId :: String
                       }
clientStop :: StetsonHandler ClientStopState
clientStop =

  Rest.handler init
  # Rest.allowedMethods (Rest.result (POST : mempty))
  # Rest.contentTypesAccepted (\req state -> Rest.result (singleton $ MimeType.json removeClient) req state)
  # Rest.resourceExists resourceExists
  # Rest.yeeha

  where
    init req = do
      let
        slotId = Bindings.slotId req
        clientId = fromMaybe' (lazyCrashIfMissing $ "client_id binding missing") $ binding (atom "client_id") req
      thisNode <- extractAddress <$> PoPDefinition.getThisServer
      Rest.initResult req { egestKey: EgestKey slotId
                          , clientId
                          }

    serviceAvailable req state = do
      isAgentAvailable <- EgestInstanceSup.isAvailable
      Rest.result isAgentAvailable req state


    resourceExists req state@{egestKey} = do
      isActive <- EgestInstance.isActive egestKey
      Rest.result isActive req state

    removeClient req state@{egestKey, clientId} =
      do
      _ <- Audit.clientStop egestKey
      _ <- stopHandler clientId
      -- _ <- EgestInstance.removeClient egestKey
      Rest.result true req state

startHandler :: String -> Effect Pid
startHandler clientId =
  let
    proc = do
      GProc.register (tuple2 (atom "test_egest_client") clientId)
      _ <- Raw.receive
      pure unit
  in
    Raw.spawn proc

stopHandler :: String -> Effect Unit
stopHandler clientId = do
  pid <- Gproc.whereIs (tuple2 (atom "test_egest_client") clientId)
  _ <- traverse ((flip Raw.send) (atom "stop")) pid
  pure unit

--------------------------------------------------------------------------------
-- Log helpers
--------------------------------------------------------------------------------
domains :: List Atom
domains = atom <$> ("Client" :  "Instance" : List.nil)

logInfo :: forall a. Logger a
logInfo = domainLog Logger.info

--logWarning :: forall a. Logger a
--logWarning = domainLog Logger.warning

domainLog :: forall a. Logger {domain :: List Atom, misc :: a} -> Logger a
domainLog = Logger.doLog domains




--------------------------------------------------------------------------------
-- Do we have any EgestInstances in this pop that have capacity for another client?
-- Yes -> If this node is one of them -> Use it (TODO - maybe load concentrate?)
--                          Otherwise -> Redirect the client to one of those servers
--  No -> Is there a stream relay already running in the pop?
--          Yes -> Does anywhere have capacity to start a egest instance
--                   Yes -> Redirect to it
--                    No -> Fail request (out of resources)
--          No -> Try to find or create a relay for this stream and also put an egest handler on the same node
-- otherwise 404
--------------------------------------------------------------------------------



  -- existingEgests <- IntraPoP.whereIsEgest slotId

  -- let
  --   thisAddress = extractAddress thisServer
  --   egestsWithCapacity  =
  --     List.filter (\(ServerLoad sl) ->
  --                   sl.load < (wrap 76.323341)
  --                 ) $ existingEgests
  -- if any (\server ->  (extractAddress server) == thisAddress) egestsWithCapacity
  -- then
  --   do
  --     _ <- EgestInstance.addClient slotId
  --     pure $ Right Local
  -- else
  --  case pickInstance egestsWithCapacity of
  --    Just egestServerAddress ->
  --      pure $ Right $ Remote (spy "remote" egestServerAddress)

  --    Nothing -> do
  --       -- does the stream even exists
  --       mAggregator <- IntraPoP.whereIsIngestAggregator slotId
  --       case spy "mAggregator" mAggregator of
  --         Nothing ->
  --           pure $ Left NotFound

  --         Just aggregator -> do
  --           mIdleServer <- IntraPoP.getIdleServer (const true)
  --           case mIdleServer of
  --             Nothing ->
  --               pure $ Left NoResource
  --             Just idleServer ->
  --               let payload = { slotId
  --                             , aggregator} :: CreateEgestPayload
  --               in
  --                 if extractAddress idleServer == thisAddress
  --                 then do
  --                   _ <- EgestInstanceSup.maybeStartAndAddClient payload
  --                   pure $ Right Local
  --                 else do
  --                   let
  --                     url = makeUrl idleServer EgestE
  --                     addr = extractAddress idleServer
  --                   _ <- crashIfLeft =<< SpudGun.postJson url payload
  --                   pure $ Right $ Remote addr

  -- where
  --   -- TODO not just head :)
  --   pickInstance = map extractAddress <<< List.head
