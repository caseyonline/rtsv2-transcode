module Bus where

import Prelude
import Effect (Effect)
import Erl.Process.Raw (Pid)
import Pinto (ServerName)

newtype SubscriptionRef msg = SubscriptionRef Pid

foreign import subscribe_ :: forall busName busMsg msg. (Pid -> SubscriptionRef busMsg) -> busName -> (busMsg -> msg) -> Effect (SubscriptionRef busMsg)
foreign import unsubscribe_ :: Pid -> Effect Unit
foreign import raise_ :: forall busName busMsg. busName -> busMsg -> Effect Unit

newtype Bus busName busMsg = Bus busName

bus :: forall busName busMsg. busName -> Bus busName busMsg
bus name = Bus name

subscribe :: forall busName busMsg state msg. ServerName state msg -> Bus busName busMsg -> (busMsg -> msg) -> Effect (SubscriptionRef busMsg)
subscribe _ (Bus name) mapper = subscribe_ SubscriptionRef name mapper

unsubscribe :: forall busMsg. SubscriptionRef busMsg -> Effect Unit
unsubscribe (SubscriptionRef pid) = unsubscribe_ pid

raise :: forall busName busMsg. Bus busName busMsg -> busMsg -> Effect Unit
raise (Bus name) msg = raise_ name msg
