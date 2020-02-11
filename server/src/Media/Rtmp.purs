module Media.Rtmp
       (
         foreignToMetadata
       ) where

import Erl.Data.List (List)
import Foreign (Foreign)
import Shared.Types.Media.Types.Rtmp (RtmpClientMetadata, emptyRtmpClientMetadata)

foreign import foreignToPursImpl :: Foreign -> (RtmpClientMetadata List) -> (RtmpClientMetadata List)

foreignToMetadata :: Foreign -> (RtmpClientMetadata List)
foreignToMetadata proplist = foreignToPursImpl proplist emptyRtmpClientMetadata
