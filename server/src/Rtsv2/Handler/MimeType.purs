module Rtsv2.Handler.MimeType
       ( json
       , text
       , llwp
       , any
       ) where

import Erl.Data.Tuple (Tuple2, tuple2)

text :: forall a. a -> Tuple2 String a
text = tuple2 "text/plain"

json :: forall a. a -> Tuple2 String a
json = tuple2 "application/json"

llwp :: forall a. a -> Tuple2 String a
llwp = tuple2 "application/vnd.id3as.media+llwp"

any :: forall a. a -> Tuple2 String a
any = tuple2 "*/*"
