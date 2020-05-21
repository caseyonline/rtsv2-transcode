module Rtsv2.Handler.Client
       ( clientStart
       , clientStop
       , ClientStartState
       ) where

import Prelude

import Data.Either (Either(..))
import Data.Newtype (unwrap)
import Data.String (Pattern(..), Replacement(..), replaceAll)
import Data.Traversable (traverse)
import Effect (Effect)
import Erl.Atom (Atom, atom)
import Erl.Cowboy.Handlers.Rest (MovedResult, moved, notMoved)
import Erl.Cowboy.Req (Req, StatusCode(..), replyWithoutBody, setHeader)
import Erl.Data.List (List, singleton, (:))
import Erl.Data.List as List
import Erl.Data.Map as Map
import Erl.Data.Tuple (tuple2)
import Erl.Process.Raw (Pid)
import Erl.Process.Raw as Raw
import Gproc as GProc
import Gproc as Gproc
import Logger (Logger)
import Logger as Logger
import Rtsv2.Agents.EgestInstance as EgestInstance
import Rtsv2.Agents.EgestInstanceSup as EgestInstanceSup
import Rtsv2.Audit as Audit
import Rtsv2.Config (LoadConfig)
import Rtsv2.Handler.MimeType as MimeType
import Rtsv2.PoPDefinition as PoPDefinition
import Rtsv2.Utils (cryptoStrongToken)
import Shared.Rtsv2.Router.Endpoint (Endpoint(..), makeUrl)
import Shared.Rtsv2.Stream (EgestKey(..), SlotId, SlotRole)
import Shared.Rtsv2.Types (CanaryState, FailureReason(..), LocalOrRemote(..), Server, extractAddress)
import Stetson (HttpMethod(..), RestResult, StetsonHandler)
import Stetson.Rest as Rest

newtype ClientStartState = ClientStartState { clientId :: String
                                            , egestResp :: (Either FailureReason (LocalOrRemote Server))
                                            }

clientStart :: LoadConfig -> CanaryState -> SlotId -> SlotRole -> StetsonHandler ClientStartState
clientStart loadConfig canary slotId slotRole =
  Rest.handler init
  # Rest.allowedMethods (Rest.result (POST : mempty))
  # Rest.contentTypesAccepted (\req state -> Rest.result (singleton $ MimeType.json acceptAny) req state)
  # Rest.resourceExists resourceExists
  # Rest.previouslyExisted previouslyExisted
  # Rest.movedTemporarily movedTemporarily
  # Rest.yeeha

  where
    init req = do
      clientId <- replaceAll (Pattern "/") (Replacement "_") <$> cryptoStrongToken 4
      thisServer <- PoPDefinition.getThisServer
      egestResp <- EgestInstanceSup.findEgest loadConfig canary slotId slotRole
      let
        req2 = setHeader "x-servedby" (unwrap $ extractAddress thisServer) req
               # setHeader "x-client-id" clientId
      Rest.initResult req2 $ ClientStartState { clientId, egestResp }

    acceptAny req state = Rest.result true req state

    resourceExists req state@(ClientStartState { egestResp, clientId}) =
      case egestResp of
        Left NotFound ->
          do
            newReq <- replyWithoutBody (StatusCode 404) Map.empty req
            Rest.stop newReq state
        Left NoResource -> do
          newReq <- replyWithoutBody (StatusCode 503) Map.empty req
          Rest.stop newReq state
        Right (Local _)  -> do
          handlerPid <- startHandler clientId
          _ <- EgestInstance.addClient handlerPid (EgestKey slotId slotRole) clientId
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
    movedTemporarily req state@(ClientStartState { egestResp}) =
      case egestResp of
        Right (Remote server) ->
          let
            url = makeUrl server (ClientStartE canary slotId slotRole)
          in
            Rest.result (moved $ unwrap url) req state
        _ ->
          Rest.result notMoved req state

type ClientStopState = { egestKey :: EgestKey
                       }
clientStop :: SlotId -> SlotRole -> String -> StetsonHandler ClientStopState
clientStop slotId slotRole clientId  =
  Rest.handler init
  # Rest.allowedMethods (Rest.result (POST : mempty))
  # Rest.contentTypesAccepted (\req state -> Rest.result (singleton $ MimeType.json removeClient) req state)
  # Rest.resourceExists resourceExists
  # Rest.yeeha

  where
    init req = do
      thisNode <- extractAddress <$> PoPDefinition.getThisServer
      Rest.initResult req { egestKey: EgestKey slotId slotRole
                          }

    resourceExists req state@{egestKey} = do
      isActive <- EgestInstance.isActive egestKey
      Rest.result isActive req state

    removeClient req state@{egestKey} =
      do
      _ <- Audit.clientStop egestKey
      _ <- stopHandler clientId
      Rest.result true req state

startHandler :: String -> Effect Pid
startHandler clientId =
  let
    proc = do
      _ <- logInfo "fake handler starting" {}
      _ <- GProc.register (tuple2 (atom "test_egest_client") clientId)
      handlerLoop
      _ <- logInfo "fake handler stopping" {}
      pure unit
  in
    Raw.spawn proc

handlerLoop :: Effect Unit
handlerLoop = do
  x <- Raw.receive
  if (x == (atom "stop")) then pure unit
    else handlerLoop

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

logInfo :: forall a. Logger (Record a)
logInfo = domainLog Logger.info

--logWarning :: forall a. Logger a
--logWarning = domainLog Logger.warning

domainLog :: forall a. Logger {domain :: List Atom, misc :: Record a} -> Logger (Record a)
domainLog = Logger.doLog domains
