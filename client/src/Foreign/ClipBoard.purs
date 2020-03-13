module Foreign.ClipBoard
       ( copyToClipboard )
       where

import Data.Unit (Unit)
import Effect (Effect)

foreign import copyToClipboard_ :: String -> Effect Unit

copyToClipboard :: String -> Effect Unit
copyToClipboard = copyToClipboard_
