module Rtsv2.Handler.TranscodeServer
       (
         transcode
       ) where

import Prelude

import Rtsv2.Agents.TranscodeNodeServer as TranscodeNodeServer
import Rtsv2.Config (LoadConfig)
import Rtsv2.Config as Config
import Effect (Effect)
import Effect.Console (log, logShow)
import Data.Maybe (Maybe(..))
import Stetson (StetsonHandler)
import Shared.Rtsv2.Types (CanaryState(..), FailureReason, Server)
import Shared.Common (Url)
import StetsonHelper (jsonResponse)


transcode :: LoadConfig -> CanaryState -> StetsonHandler (Maybe (String))
transcode loadConfig canary =
  jsonResponse getUrls

  where
    getUrls =
      do
        config <- Config.llnwApiConfig
        {} <- TranscodeNodeServer.transcode "Movie" "125k"
        {} <- TranscodeNodeServer.transcode "Movie" "500k"
        {} <- TranscodeNodeServer.transcode "Movie" "750k"
        pure $ Just "*** Transcoding ***"
