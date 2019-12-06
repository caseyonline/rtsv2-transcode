module Rtsv2.Endpoints.MimeType
       (
         json
       ) where

import Erl.Data.Tuple (Tuple2, tuple2)

text :: forall a. a -> Tuple2 String a
text = tuple2 "text/plain"

json :: forall a. a -> Tuple2 String a
json = tuple2 "application/json"
