module Rtsv2.Handler.Client
       ( clientStart
       , clientStop
       , State
       ) where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (fromMaybe')
import Data.Newtype (unwrap, wrap)
import Effect (Effect)
import Erl.Atom (Atom, atom)
import Erl.Cowboy.Handlers.Rest (MovedResult, moved, notMoved)
import Erl.Cowboy.Req (Req, StatusCode(..), binding, replyWithoutBody, setHeader)
import Erl.Data.List (List, singleton, (:))
import Erl.Data.List as List
import Erl.Data.Map as Map
import Logger (Logger, spy)
import Logger as Logger
import Rtsv2.Agents.Egest.Locator (findEgestForStream)
import Rtsv2.Agents.EgestInstance as EgestInstance
import Rtsv2.Agents.EgestInstanceSup as EgestInstanceSup
import Rtsv2.Audit as Audit
import Rtsv2.Handler.MimeType as MimeType
import Rtsv2.PoPDefinition as PoPDefinition
import Rtsv2.Router.Endpoint (Endpoint(..), makeUrl)
import Rtsv2.Router.Endpoint as RoutingEndpoint
import Rtsv2.Router.Parser as Routing
import Shared.Stream (StreamId(..))
import Shared.Types (EgestLocation(..), FailureReason(..), Server, ServerAddress(..), extractAddress)
import Shared.Utils (lazyCrashIfMissing)
import Stetson (HttpMethod(..), RestResult, StetsonHandler)
import Stetson.Rest as Rest

data ClientStartState
  = StreamNotAvailable
  | Foo  { streamId :: StreamId
         , isIngestAvailable :: Boolean
         , currentNodeHasEdge :: Boolean
         , thisServer :: Server
         , currentEdgeLocations :: List ServerAddress
         }

newtype State = State { streamId :: StreamId
                      , egestResp :: (Either FailureReason EgestLocation)
                      }

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



  -- existingEgests <- IntraPoP.whereIsEgest streamId

  -- let
  --   thisAddress = extractAddress thisServer
  --   egestsWithCapacity  =
  --     List.filter (\(ServerLoad sl) ->
  --                   sl.load < (wrap 76.323341)
  --                 ) $ existingEgests
  -- if any (\server ->  (extractAddress server) == thisAddress) egestsWithCapacity
  -- then
  --   do
  --     _ <- EgestInstance.addClient streamId
  --     pure $ Right Local
  -- else
  --  case pickInstance egestsWithCapacity of
  --    Just egestServerAddress ->
  --      pure $ Right $ Remote (spy "remote" egestServerAddress)

  --    Nothing -> do
  --       -- does the stream even exists
  --       mAggregator <- IntraPoP.whereIsIngestAggregator streamId
  --       case spy "mAggregator" mAggregator of
  --         Nothing ->
  --           pure $ Left NotFound

  --         Just aggregator -> do
  --           mIdleServer <- IntraPoP.getIdleServer (const true)
  --           case mIdleServer of
  --             Nothing ->
  --               pure $ Left NoResource
  --             Just idleServer ->
  --               let payload = { streamId
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




clientStart :: StetsonHandler State
clientStart =
  Rest.handler init
  # Rest.allowedMethods (Rest.result (POST : mempty))
  # Rest.contentTypesAccepted (\req state -> Rest.result (singleton $ MimeType.json acceptAny) req state)
  # Rest.resourceExists resourceExists
  # Rest.previouslyExisted previouslyExisted
  # Rest.movedTemporarily movedTemporarily
  # Rest.yeeha

  where
    init req =
      let
        streamId = spy "clientStartInit" $ StreamId $ fromMaybe' (lazyCrashIfMissing "stream_id binding missing") $ binding (atom "stream_id") req
      in
        do

          thisServer <- PoPDefinition.getThisServer
          egestResp <- findEgestForStream  streamId
          let
            req2 = setHeader "x-servedby" (unwrap $ extractAddress thisServer) req
            _ = spy "egestResp" egestResp
          Rest.initResult req2 $ State { streamId, egestResp }

    acceptAny req state = Rest.result true req state

    resourceExists req state@(State {egestResp}) =
      case egestResp of
        Left NotFound ->
          do
            newReq <- replyWithoutBody (StatusCode 404) Map.empty req
            Rest.stop newReq state
        Left NoResource -> do
          newReq <- replyWithoutBody (StatusCode 502) Map.empty req
          Rest.stop newReq state
        Right Local ->
          Rest.result true req state
        Right (Remote _) ->
          Rest.result false req state

    previouslyExisted req state@(State {egestResp}) =
      let resp = case egestResp of
            Right (Remote _) -> true
            _ -> false
      in
       Rest.result resp req state

    movedTemporarily :: Req -> State -> Effect (RestResult MovedResult State)
    movedTemporarily req state@(State {streamId, egestResp}) =
      case spy "moved" egestResp of
        Right (Remote server) ->
          let
            url = makeUrl server (ClientStartE "canary" streamId)
            -- path = Routing.printUrl RoutingEndpoint.endpoint
            -- url = spy "url" $ "http://" <> unwrap addr <> ":3000" <> path
          in
            Rest.result (moved $ unwrap url) req state
        _ ->
          Rest.result notMoved req state





type ClientStopState = { streamId :: StreamId
                       }
clientStop :: StetsonHandler ClientStopState
clientStop =

  Rest.handler init
  # Rest.allowedMethods (Rest.result (POST : mempty))
  # Rest.contentTypesAccepted (\req state -> Rest.result (singleton $ MimeType.json removeClient) req state)
  # Rest.resourceExists resourceExists
  # Rest.yeeha

  where
    init req =
      let streamId = StreamId $ fromMaybe' (lazyCrashIfMissing "stream_id binding missing") $ binding (atom "stream_id") req
      in
       do
         thisNode <- extractAddress <$> PoPDefinition.getThisServer
         Rest.initResult req { streamId : spy "init" streamId
                             }

    acceptAny req state = Rest.result true req  $ spy "accept" state

    serviceAvailable req state = do
      isAgentAvailable <- EgestInstanceSup.isAvailable
      Rest.result isAgentAvailable req  $ spy "savail" state


    resourceExists req state@{streamId} = do
      isActive <- EgestInstance.isActive $ spy "exists" streamId
      Rest.result isActive req state

    removeClient req state@{streamId} =
      let _ = spy "remove" streamId
      in do
      _ <- Audit.clientStop streamId
      _ <- EgestInstance.removeClient $ spy "remove" streamId
      Rest.result true req state

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
