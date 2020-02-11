module Media.SourceDetails
       (
         foreignToSourceInfo
       ) where

import Erl.Data.List (List)
import Foreign (Foreign)
import Shared.Types.Media.Types.SourceDetails (SourceInfo)

foreign import foreignToPursImpl :: Foreign -> (SourceInfo List)

foreignToSourceInfo :: Foreign -> SourceInfo List
foreignToSourceInfo = foreignToPursImpl
