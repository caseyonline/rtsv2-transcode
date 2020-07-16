module Rtsv2.Handler.TestServer
       (
         ping
       ) where

import Prelude

import Rtsv2.Agents.TestNodeServer as TestNodeServer
import Rtsv2.Config (LoadConfig)
import Rtsv2.Config as Config
import Effect (Effect)
import Effect.Console (log, logShow)
import Data.Maybe (Maybe(..))
import Stetson (StetsonHandler)
import Shared.Rtsv2.Types (CanaryState(..), FailureReason, Server)
import Shared.Common (Url)
import StetsonHelper (jsonResponse)


ping :: LoadConfig -> CanaryState -> StetsonHandler (Maybe (String))
ping loadConfig canary =
  jsonResponse getUrls

  where
    getUrls =
      do
        config <- Config.llnwApiConfig
        {} <- TestNodeServer.transcode "Movie" "123k"
        pure $ Just "some blah blah string"
