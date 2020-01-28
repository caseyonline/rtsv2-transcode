module Rtsv2App.Data.Utils where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Decode.Struct.Tolerant ((.::))
import Data.Argonaut.Decode.Struct.Tolerant as Tolerant
import Data.Either (Either)

decodeAt :: forall a. Tolerant.DecodeJson a => String -> Json -> Either String a
decodeAt key = (_ .:: key) <=< decodeJson
