module Main where

import Prelude
import Affjax as AX
import Affjax.ResponseFormat as ResponseFormat
import Affjax.StatusCode (StatusCode(..))
import Data.Argonaut.Core as J
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (sequence)
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
        before_ (launchNodes [ "node3000" ]) do
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

launchNodes :: Array String -> Aff Unit
launchNodes sysconfigs = do
  void $ sequence $ ((\sc -> runProc "./scripts/startNode.sh" [ sc ]) <$> sysconfigs)

stopNodes :: Aff Unit
stopNodes = runProc "./scripts/stopNodes.sh" []
