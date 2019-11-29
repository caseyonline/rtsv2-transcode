module Main where

import Prelude

import Affjax as AX
import Affjax.ResponseFormat as ResponseFormat
import Affjax.StatusCode (StatusCode(..))
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Identity (Identity(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (un)
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (traverse_)
import Effect (Effect)
import Effect.Aff (launchAff_, Aff)
import OsCmd (runProc)
import Test.Spec (after_, before_, describe, it)
import Test.Spec.Assertions (fail)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec, runSpecT)


type TestNode =  { nodeName :: String
                 , vlan :: String
                 , addr :: String
                 , sysConfig :: String
                 }

node :: String -> String -> String -> String -> TestNode
node nodeName vlan addr sysConfig = {nodeName, vlan, addr, sysConfig}



main :: Effect Unit
main =
  launchAff_ $ un Identity $ runSpecT testConfig [consoleReporter] do
        before_ (launchNodes [ node "node3000" "vlan201" "172.16.169.1" "scripts/env/steve.data/sys.config" ]) do
          after_ stopNodes do
            describe "single edge agent running" do
              it "client requests stream which is not being ingested" do
                result <- AX.request (AX.defaultRequest { url = "http://localhost:3000/poc/api/client/canary/edge/stream1/connect", method = Left GET, responseFormat = ResponseFormat.string })
                case result of
                  Left err -> do
                    fail $ "GET /api response failed to decode: " <> AX.printError err
                  Right response -> do
                    case response.status of
                      StatusCode 404 -> pure unit
                      sc -> fail $ "Unexpected statuscode" <> show sc
        before_ (launchNodes [ node "node3000" "vlan201" "172.16.169.1" "scripts/env/steve.data/sys.config" ]) do
          after_ stopNodes do
            describe "multiple edge agents running" do
              it "client requests stream which is not being ingested" do
                result <- AX.request (AX.defaultRequest { url = "http://localhost:3000/poc/api/client/canary/edge/stream1/connect", method = Left GET, responseFormat = ResponseFormat.string })
                case result of
                  Left err -> do
                    fail $ "GET /api response failed to decode: " <> AX.printError err
                  Right response -> do
                    case response.status of
                      StatusCode 404 -> pure unit
                      sc -> fail $ "Unexpected statuscode" <> show sc
  where
    testConfig = { slow: Milliseconds 5000.0, timeout: Just (Milliseconds 10000.0), exit: false }


sessionName:: String
sessionName = "testSession"

launchNodes :: Array TestNode -> Aff Unit
launchNodes sysconfigs = do
  _ <- runProc "./scripts/startSession.sh" [sessionName]
  traverse_ (\tn -> runProc "./scripts/startNode.sh"
                    [ sessionName
                    , tn.nodeName
                    , tn.vlan
                    , tn.addr
                    , tn.sysConfig
                    ]) sysconfigs

stopNodes :: Aff Unit
stopNodes = do
  _ <- runProc "./scripts/stopNodes.sh" []
  runProc "./scripts/stopSession.sh" [sessionName]
