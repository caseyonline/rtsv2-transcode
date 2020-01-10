module Rtsv2.Handler.MimeType
       ( json
       , text
       , any
       ) where

import Erl.Data.Tuple (Tuple2, tuple2)

text :: forall a. a -> Tuple2 String a
text = tuple2 "text/plain"

json :: forall a. a -> Tuple2 String a
json = tuple2 "application/json"

any :: forall a. a -> Tuple2 String a
any = tuple2 "*/*"
