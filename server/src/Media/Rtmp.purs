module Media.Rtmp
       (
         foreignToMetadata
       ) where

import Data.Function.Uncurried (Fn2, runFn2)
import Erl.Data.List (List)
import Foreign (Foreign)
import Shared.Types.Media.Types.Rtmp (RtmpClientMetadata, emptyRtmpClientMetadata)

foreign import foreignToPursImpl :: Fn2 Foreign (RtmpClientMetadata List) (RtmpClientMetadata List)

foreignToMetadata :: Foreign -> (RtmpClientMetadata List)
foreignToMetadata proplist = runFn2 foreignToPursImpl proplist emptyRtmpClientMetadata
