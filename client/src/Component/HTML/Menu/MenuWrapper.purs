module Rtsv2App.Component.HTML.Menu.MenuWrapper where

import Prelude

import Control.Monad.Reader.Trans (class MonadAsk)
import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Rtsv2App.Capability.Navigate (class Navigate)
import Rtsv2App.Component.HTML.Menu.MainHelper (MenuState(..), closeSecondaryMenu, openSecondaryMenu)
import Rtsv2App.Component.HTML.Menu.MenuMain as MM
import Rtsv2App.Component.HTML.Menu.MenuSecondary as MS
import Rtsv2App.Data.Profile (Profile)
import Rtsv2App.Data.Route (Route)
import Rtsv2App.Env (PoPDefEnv)
import Shared.Types (PoPName(..))

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------
  
data Action
  = HandleMenuState MenuState
  | Receive Input

type State =
  { currentUser  :: Maybe Profile
  , route        :: Route
  , isMenuClosed :: Boolean
  , curPopName   :: Maybe PoPName
  }

type Input =
  { currentUser  :: Maybe Profile
  , route        :: Route
  , curPopName   :: Maybe PoPName
  }

type ChildSlots =
  ( menuMain      :: MM.Slot Unit
  , menuSecondary :: MS.Slot Unit
  )

type Slot
  = H.Slot (Const Void) Void

-------------------------------------------------------------------------------
-- Component
-------------------------------------------------------------------------------
component
  :: forall m r
   . MonadAff m
  => MonadAsk { popDefEnv :: PoPDefEnv | r } m
  => Navigate m
  => H.Component HH.HTML (Const Void) Input Void m
component = H.mkComponent
  { initialState: initialState
  , render
  , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      , receive = Just <<< Receive
      }
  }
  where
  initialState :: Input -> State
  initialState { currentUser, route, curPopName } =
    { currentUser
    , route
    , isMenuClosed: true
    , curPopName
    }
    
  handleAction :: Action -> H.HalogenM State Action ChildSlots Void m Unit
  handleAction = case _ of
    Receive { currentUser, route, curPopName } -> do
      H.put { currentUser
            , route
            , isMenuClosed: true
            , curPopName
            }

    HandleMenuState menuState -> do
      st <- H.get
      case menuState of
        OpenMenu -> do
          H.modify_ _ { isMenuClosed = false }
          _ <- liftEffect $ openSecondaryMenu
          pure unit
          
        CloseMenu -> do
          H.modify_ _ { isMenuClosed = true }
          _ <- liftEffect $ closeSecondaryMenu
          pure unit
      
  render :: State -> H.ComponentHTML Action ChildSlots m
  render state =
    HH.div_
    [ HH.slot (SProxy :: _ "menuMain") unit MM.component state (Just <<< HandleMenuState)
    , HH.slot (SProxy :: _ "menuSecondary") unit MS.component state (Just <<< HandleMenuState) 
    ]

