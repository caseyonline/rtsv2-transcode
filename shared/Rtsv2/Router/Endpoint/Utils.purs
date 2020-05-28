module Shared.Rtsv2.Router.Endpoint.Utils
       ( makeUrl
       , makeWsUrl
       ) where

import Prelude

import Data.Newtype (wrap)
import Effect (Effect)
import Rtsv2.Env as Env
import Shared.Common (Url)

makeUrl :: String -> Int -> String -> Effect Url
makeUrl host port path = do
  isProxied <- Env.isProxied
  pure $ if isProxied then
           wrap $ "https://" <> host <> path
         else
           wrap $ "http://" <> host <> ":" <> (show port) <> path

makeWsUrl :: String -> Int -> String -> Effect Url
makeWsUrl host port path = do
  isProxied <- Env.isProxied
  pure $ if isProxied then
           wrap $ "wss://" <> host <> path
         else
           wrap $ "ws://" <> host <> ":" <> (show port) <> path
