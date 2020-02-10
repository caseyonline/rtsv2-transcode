module Media.SourceDetails
       (
         foreignToSourceInfo
       ) where

import Data.Function.Uncurried (Fn1)
import Erl.Data.List (List)
import Foreign (Foreign)
import Shared.Types.Media.Types.SourceDetails (SourceInfo)
import Unsafe.Coerce (unsafeCoerce)

foreign import foreignToPursImpl :: Fn1 Foreign (SourceInfo List)

foreignToSourceInfo :: Foreign -> SourceInfo List
foreignToSourceInfo = unsafeCoerce 1
