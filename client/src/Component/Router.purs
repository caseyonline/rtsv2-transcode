module Rtsv2App.Component.Router where

import Prelude

import Component.HOC.Connect (WithCurrentUser)
import Component.HOC.Connect as Connect
import Control.Monad.Reader (class MonadAsk)
import Data.Either (hush)
import Data.Foldable (elem)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Symbol (SProxy(..))
import Effect.Aff.Class (class MonadAff)
import Halogen (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Routing.Duplex as RD
import Routing.Hash (getHash)
import Rtsv2App.Capability.LogMessages (class LogMessages)
import Rtsv2App.Capability.Navigate (class Navigate, navigate)
import Rtsv2App.Capability.Now (class Now)
import Rtsv2App.Capability.Resource.Api (class ManageApi)
import Rtsv2App.Capability.Resource.User (class ManageUser)
import Rtsv2App.Component.Utils (OpaqueSlot)
import Rtsv2App.Data.Profile (Profile)
import Rtsv2App.Data.Route (Route(..), routeCodec)
import Rtsv2App.Env (UrlEnv, UserEnv, PoPDefEnv)
import Rtsv2App.Page.Dashboard as Dashboard
import Rtsv2App.Page.Login as Login
import Rtsv2App.Page.PoPHome as PoPHome
import Rtsv2App.Page.Register as Register
import Rtsv2App.Page.Settings as Settings

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------
type State =
  { route :: Maybe Route 
  , currentUser :: Maybe Profile
  }

data Query a
  = Navigate Route a

data Action 
  = Initialize 
  | Receive { | WithCurrentUser () }

type ChildSlots = 
  ( dashboard :: OpaqueSlot Unit
  , popHome :: OpaqueSlot Unit
  , login :: OpaqueSlot Unit
  , register :: OpaqueSlot Unit
  , settings :: OpaqueSlot Unit
  )

-------------------------------------------------------------------------------
-- Router Component
-------------------------------------------------------------------------------
component
  :: forall m r
   . MonadAff m
  => MonadAsk { userEnv :: UserEnv, urlEnv :: UrlEnv, popDefEnv :: PoPDefEnv | r } m
  => Now m
  => LogMessages m
  => Navigate m
  => ManageUser m
  => ManageApi m
  => H.Component HH.HTML Query {} Void m
component = Connect.component $ H.mkComponent
  { initialState: \ { currentUser } -> { route: Nothing, currentUser }
  , render
  , eval: H.mkEval $ H.defaultEval 
      { handleQuery = handleQuery 
      , handleAction = handleAction
      , receive = Just <<< Receive
      , initialize = Just Initialize
      }
  }
  where 
  handleAction :: Action -> H.HalogenM State Action ChildSlots Void m Unit
  handleAction = case _ of
    Initialize -> do
      -- get the route the user landed on
      initialRoute <- hush <<< (RD.parse routeCodec) <$> liftEffect getHash
      -- navigate to the new route (also setting the hash)
      navigate $ fromMaybe Dashboard initialRoute
    
    Receive { currentUser } -> do
      H.modify_ _ { currentUser = currentUser }

  handleQuery :: forall a. Query a -> H.HalogenM State Action ChildSlots Void m (Maybe a)
  handleQuery = case _ of
    Navigate dest a -> do
      { route, currentUser } <- H.get
      -- don't re-render unnecessarily if the route is unchanged
      when (route /= Just dest) do
        -- don't change routes if there is a logged-in user trying to access
        -- a route only meant to be accessible to a not-logged-in session
        case (isJust currentUser && dest `elem` [ Login, Register ]) of
          false -> H.modify_ _ { route = Just dest }
          _ -> pure unit
      pure (Just a)

  -- Display the login page instead of the expected page if there is no current user; a simple 
  -- way to restrict access.
  authorize :: Maybe Profile -> H.ComponentHTML Action ChildSlots m -> H.ComponentHTML Action ChildSlots m
  authorize mbProfile html = case mbProfile of
    Nothing ->
      HH.slot (SProxy :: _ "login") unit Login.component { redirect: false } absurd
    Just _ ->
      html

  -- connecting the routes to the components
  render :: State -> H.ComponentHTML Action ChildSlots m
  render { route, currentUser } = case route of
    Just r -> case r of
      Dashboard ->
        HH.slot (SProxy :: _ "dashboard") unit Dashboard.component {} absurd
          # authorize currentUser
      PoPHome popName  ->
        HH.slot (SProxy :: _ "popHome") unit PoPHome.component { popName: popName } absurd
          # authorize currentUser
      Login -> 
        HH.slot (SProxy :: _ "login") unit Login.component { redirect: true } absurd
      Register ->
        HH.slot (SProxy :: _ "register") unit Register.component {} absurd
      Settings -> 
        HH.slot (SProxy :: _ "settings") unit Settings.component {} absurd
          # authorize currentUser
    Nothing ->
      HH.div_ [ HH.text "Oh no! That page wasn't found." ]
