module Main where

import Prelude
import Affjax as AX
import Affjax.ResponseFormat as ResponseFormat
import Affjax.StatusCode (StatusCode(..))
import Data.Argonaut.Core as J
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Time.Duration (Milliseconds(..))
import Debug.Trace (spy)
import Effect (Effect)
import Effect.Aff (launchAff_, delay, Aff)
import Effect.Class.Console (log)
import OsCmd (runProc)
import Test.Spec (before_, describe, it, pending)
import Test.Spec.Assertions (fail, shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main =
  launchAff_
    $ runSpec [ consoleReporter ] do
        before_ launchEdge do
          describe "client requests stream" do
            it "fails if no edge agent" do
              result <- AX.request (AX.defaultRequest { url = "http://localhost:3000/poc/api/client/canary/foo", method = Left GET, responseFormat = ResponseFormat.string })
              case spy "result is" result of
                Left err -> do
                  log $ "GET /api response failed to decode: " <> AX.printError err
                  fail "HTTP Error"
                Right response -> do
                  -- log $ "GET /api response: " <> response.body
                  case response.status of
                    StatusCode 200 -> pure unit
                    sc -> fail $ "Unexpected statuscode" <> show sc

launchEdge :: Aff Unit
launchEdge = runProc "./scripts/startNode.sh" []
