module Logger
       (
         emergency
       , alert
       , critical
       , error
       , warning
       , notice
       , info
       , debug
       ) where

import Effect (Effect)
import Foreign (Foreign)

foreign import emergency_ :: forall a. String -> a -> Effect Foreign
foreign import alert_ :: forall a. String -> a -> Effect Foreign
foreign import critical_ :: forall a. String -> a -> Effect Foreign
foreign import error_ :: forall a. String -> a -> Effect Foreign
foreign import warning_ :: forall a. String -> a -> Effect Foreign
foreign import notice_ :: forall a. String -> a -> Effect Foreign
foreign import info_ :: forall a. String -> a -> Effect Foreign
foreign import debug_ :: forall a. String -> a -> Effect Foreign

emergency :: forall a. String -> a -> Effect Foreign
emergency = emergency_

alert :: forall a. String -> a -> Effect Foreign
alert = alert_

critical :: forall a. String -> a -> Effect Foreign
critical = critical_

error :: forall a. String -> a -> Effect Foreign
error = error_

warning :: forall a. String -> a -> Effect Foreign
warning = warning_

notice :: forall a. String -> a -> Effect Foreign
notice = notice_

info :: forall a. String -> a -> Effect Foreign
info = info_

debug :: forall a. String -> a -> Effect Foreign
debug = debug_
