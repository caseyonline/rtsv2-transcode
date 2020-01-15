module Component.HOC.Connect where

import Prelude

import Control.Monad.Reader (class MonadAsk, asks)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Ref as Ref
import Halogen (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Prim.Row as Row
import Record as Record
import Rtsv2App.Component.Utils (busEventSource)
import Rtsv2App.Data.Profile (Profile)
import Rtsv2App.Env (UserEnv)

data Action output
  = Initialize
  | HandleUserBus (Maybe Profile)
  | Emit output

type WithCurrentUser r =
  ( currentUser :: Maybe Profile | r )

type ChildSlots query output =
  ( inner :: H.Slot query output Unit )

_inner = SProxy :: SProxy "inner"

component
  :: forall query input output m r
   . MonadAff m
  => MonadAsk { userEnv :: UserEnv | r } m
  => Row.Lacks "currentUser" input 
  => H.Component HH.HTML query { | WithCurrentUser input } output m
  -> H.Component HH.HTML query { | input } output m
component innerComponent = 
  H.mkComponent
    { initialState: Record.insert (SProxy :: _ "currentUser") Nothing
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , handleQuery = handleQuery
        , initialize = Just Initialize
        }
    }
  where
  handleAction = case _ of
    Initialize -> do
      { currentUser, userBus } <- asks _.userEnv
      _ <- H.subscribe (HandleUserBus <$> busEventSource userBus)
      mbProfile <- liftEffect $ Ref.read currentUser
      H.modify_ _ { currentUser = mbProfile }

    HandleUserBus mbProfile ->
      H.modify_ _ { currentUser = mbProfile }

    Emit output ->
      H.raise output
  
  handleQuery :: forall a. query a -> H.HalogenM _ _ _ _ _ (Maybe a)
  handleQuery = H.query _inner unit

  render state =
    HH.slot _inner unit innerComponent state (Just <<< Emit)
