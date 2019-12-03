module Bus where

import Prelude 
import Effect (Effect)
import Erl.Process.Raw (Pid)
import Pinto (ServerName(..))

newtype SubscriptionRef msg = SubscriptionRef Pid                                               

foreign import subscribe_ :: forall busMsg msg. (Pid -> SubscriptionRef busMsg) -> String -> (busMsg -> msg) -> Effect (SubscriptionRef busMsg)
foreign import unsubscribe_ :: Pid -> Effect Unit
foreign import raise_ :: forall msg. String -> msg -> Effect Unit

newtype Bus msg = Bus String 

bus :: forall msg. String -> Bus msg
bus name = Bus name

subscribe :: forall busMsg state msg. ServerName state msg -> Bus busMsg -> (busMsg -> msg) -> Effect (SubscriptionRef busMsg)
subscribe _ (Bus name) mapper = subscribe_ SubscriptionRef name mapper

unsubscribe :: forall busMsg. SubscriptionRef busMsg -> Effect Unit
unsubscribe (SubscriptionRef pid) = unsubscribe_ pid

raise :: forall busMsg. Bus busMsg -> busMsg -> Effect Unit
raise (Bus name) msg = raise_ name msg
