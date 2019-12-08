module Main where

import Prelude

import Affjax (Error, Response)
import Affjax as AX
import Affjax.ResponseFormat as ResponseFormat
import Affjax.StatusCode (StatusCode(..))
import Data.Either (Either(..))
import Data.Identity (Identity(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (un)
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (traverse_)
import Debug.Trace (spy)
import Effect (Effect)
import Effect.Aff (Aff, delay, launchAff_, throwError)
import Effect.Exception (error)
import OsCmd (runProc)
import Test.Spec (after_, before_, describe, it)
import Test.Spec.Assertions (fail)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpecT)


type TestNode =  { vlan :: String
                 , addr :: String
                 , sysConfig :: String
                 }

node :: String -> String -> String -> TestNode
node vlan addr sysConfig = {vlan, addr, sysConfig}



main :: Effect Unit
main =
  launchAff_ $ un Identity $ runSpecT testConfig [consoleReporter] do
    before_ (do
                _ <- stopNodes
                launchNodes [
                  node "vlan101" "172.16.169.1" "test/config/sys.config"
                  , node "vlan102" "172.16.169.2" "test/config/sys.config"
                  , node "vlan201" "172.16.170.1" "test/config/sys.config"
                  , node "vlan202" "172.16.170.2" "test/config/sys.config"
                  ]) do
      after_ stopNodes do
        describe "two node setup" do
          it "client requests stream on ingest node" do
            let
              node1ingestStart = "http://172.16.169.1:3000/api/client/:canary/ingest/stream1/low/start"
              node1edgeUrl = "http://172.16.169.1:3000/api/client/canary/edge/stream1/connect"
            _ <- assertStatusCode 404 =<< AX.get ResponseFormat.string node1edgeUrl
            _ <- assertStatusCode 200 =<< AX.get ResponseFormat.string node1ingestStart
            _ <- delay (Milliseconds 500.0)
            _ <- assertStatusCode 200 =<< AX.get ResponseFormat.string node1edgeUrl
            pure unit
          it "client requests stream on non-ingest node" do
            let
              node1ingestStart = "http://172.16.169.1:3000/api/client/:canary/ingest/stream1/low/start"
              node2edgeUrl = "http://172.16.169.2:3000/api/client/canary/edge/stream1/connect"
            _ <- assertStatusCode 404 =<< AX.get ResponseFormat.string node2edgeUrl
            _ <- assertStatusCode 200 =<< AX.get ResponseFormat.string node1ingestStart
            _ <- delay (Milliseconds 1000.0)
            _ <- assertStatusCode 200 =<< AX.get ResponseFormat.string node2edgeUrl
            pure unit
          it "client requests stream on other pop" do
            let
              node1ingestStart = "http://172.16.169.1:3000/api/client/:canary/ingest/stream1/low/start"
              node2edgeUrl = "http://172.16.170.2:3000/api/client/canary/edge/stream1/connect"
            _ <- assertStatusCode 404 =<< AX.get ResponseFormat.string node2edgeUrl
            _ <- assertStatusCode 200 =<< AX.get ResponseFormat.string node1ingestStart
            _ <- delay (Milliseconds 2000.0)
            _ <- assertStatusCode 200 =<< AX.get ResponseFormat.string node2edgeUrl
            pure unit

  where
    testConfig = { slow: Milliseconds 5000.0, timeout: Just (Milliseconds 20000.0), exit: false }

assertStatusCode :: forall a. Int -> Either Error (Response a) -> Aff (Response a)
assertStatusCode expectedCode either =
  case either of
    Right response@{status : StatusCode resultCode} | resultCode == expectedCode ->
      pure response
    Right response@{status : StatusCode resultCode} ->
      throwError $ error $ "Unexpected statuscode " <> show response.status
    Left err ->
      throwError $ error $ "GET /api response failed to decode: " <> AX.printError err

sessionName:: String
sessionName = "testSession"

launchNodes :: Array TestNode -> Aff Unit
launchNodes sysconfigs = do
  _ <- runProc "./scripts/startSession.sh" [sessionName]
  _ <- traverse_ (\tn -> runProc "./scripts/startNode.sh"
                         [ sessionName
                         , tn.addr
                         , tn.vlan
                         , tn.addr
                         , tn.sysConfig
                         ]) sysconfigs

  traverse_ (\tn -> runProc "./scripts/waitForNode.sh"
                    [ tn.addr
                    ]) sysconfigs

stopNodes :: Aff Unit
stopNodes = do
  _ <- runProc "./scripts/stopNodes.sh" []
  runProc "./scripts/stopSession.sh" [sessionName]
