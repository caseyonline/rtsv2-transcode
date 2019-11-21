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
import Test.Spec (after_, before_, describe, it, pending)
import Test.Spec.Assertions (fail, shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main =
  launchAff_
    $ runSpec [ consoleReporter ] do
        before_ launchNode do
          after_ stopNode do
            describe "client requests stream" do
              it "edge agent is present" do
                result <- AX.request (AX.defaultRequest { url = "http://localhost:3000/poc/api/client/canary/foo", method = Left GET, responseFormat = ResponseFormat.string })
                case result of
                  Left err -> do
                    fail $ "GET /api response failed to decode: " <> AX.printError err
                  Right response -> do
                    case response.status of
                      StatusCode 200 -> pure unit
                      sc -> fail $ "Unexpected statuscode" <> show sc

launchNode :: Aff Unit
launchNode = runProc "./scripts/startNode.sh" []

stopNode :: Aff Unit
stopNode = runProc "./scripts/stopNode.sh" []
