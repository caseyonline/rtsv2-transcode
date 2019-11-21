module WebRTC where

import Data.String as String
import Data.Unit (Unit)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Prelude (bind, discard, (>))
import Test.Unit.Assert as Assert
import Toppokki as T

main :: Effect Unit
main = launchAff_ do
  browser <- T.launch {}
  page <- T.newPage browser
  T.goto (T.URL "http://localhost:300/test/alive") page
  content <- T.content page
  Assert.assert "content is non-empty string" (String.length content > 0)
  T.close browser
