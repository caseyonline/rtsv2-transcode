module Rtsv2App.Component.Router where

import Prelude

import Component.HOC.Connect (WithCurrentUser)
import Component.HOC.Connect as Connect
import Control.Monad.Reader (class MonadAsk)
import Data.Array (snoc)
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
import Rtsv2App.Component.HTML.NotificationMain as NotificationMain
import Rtsv2App.Component.HTML.Utils (css_)
import Rtsv2App.Component.Utils (Notification, NotificationContent, NotificationMessage(..), OpaqueSlot)
import Rtsv2App.Data.Profile (Profile)
import Rtsv2App.Data.Route (Route(..), routeCodec)
import Rtsv2App.Env (UrlEnv, UserEnv, PoPDefEnv)
import Rtsv2App.Page.Dashboard as Dashboard
import Rtsv2App.Page.Login as Login
import Rtsv2App.Page.PoPDashboard as PoPDashboard
import Rtsv2App.Page.Register as Register
import Rtsv2App.Page.Settings as Settings

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------
type State =
  { prevRoute     :: Maybe Route
  , route         :: Maybe Route
  , currentUser   :: Maybe Profile
  , notifications :: (Array (Notification NotificationContent))
  }

data Query a
  = Navigate Route a

data Action 
  = Initialize 
  | Receive { | WithCurrentUser () }
  | UpdateNotifications NotificationMessage


type ChildSlots = 
  ( dashboard     :: OpaqueSlot Unit
  , popHome       :: OpaqueSlot Unit
  , login         :: OpaqueSlot Unit
  , register      :: OpaqueSlot Unit
  , settings      :: OpaqueSlot Unit
  , notFound      :: OpaqueSlot Unit
  -- non route related slot for notifications
  , notifications :: OpaqueSlot Unit
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
  { initialState:
      \ { currentUser } -> { prevRoute: Nothing
                           , route: Nothing
                           , currentUser
                           , notifications: mempty
                           }
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
      navigate $ fromMaybe DashboardR initialRoute
    
    Receive { currentUser } -> do
      H.modify_ _ { currentUser = currentUser }

    UpdateNotifications n -> do
      st <- H.get
      case n of
        NMessages notifications -> do
          H.modify_ _ { notifications = notifications }

        NSingleMessage notification -> do
          H.modify_ _ { notifications = snoc st.notifications notification }

  handleQuery :: forall a. Query a -> H.HalogenM State Action ChildSlots Void m (Maybe a)
  handleQuery = case _ of
    Navigate dest a -> do
      { route, currentUser } <- H.get
      -- don't re-render unnecessarily if the route is unchanged
      when (route /= Just dest) do
        -- don't change routes if there is a logged-in user trying to access
        -- a route only meant to be accessible to a not-logged-in session
        case (isJust currentUser && dest `elem` [ LoginR, RegisterR ]) of
          false -> H.modify_ _ { route = Just dest, prevRoute = route }
          _ -> pure unit

      pure (Just a)

  -- Display the login page instead of the expected page if there is no current user; a simple 
  -- way to restrict access.
  authorize :: Maybe Profile -> H.ComponentHTML Action ChildSlots m -> H.ComponentHTML Action ChildSlots m
  authorize mbProfile html = case mbProfile of
    Nothing ->
      HH.slot (SProxy :: _ "login") unit Login.component { redirect: false } (Just <<< UpdateNotifications)
    Just _ ->
      html

-------------------------------------------------------------------------------
-- Render - connecting the routes to the components
-------------------------------------------------------------------------------
  render :: State -> H.ComponentHTML Action ChildSlots m
  render { route, currentUser, prevRoute, notifications } = case route of
    Just r -> 
      HH.div_
      [ case r of
          DashboardR ->
            HH.slot (SProxy :: _ "dashboard") unit Dashboard.component { currentUser } (Just <<< UpdateNotifications)
              # authorize currentUser
          PoPDashboardR popName  ->
            HH.slot (SProxy :: _ "popHome") unit PoPDashboard.component { popName, prevRoute } (Just <<< UpdateNotifications)
              # authorize currentUser
          LoginR ->
            HH.slot (SProxy :: _ "login") unit Login.component { redirect: true } (Just <<< UpdateNotifications)
          RegisterR ->
            HH.slot (SProxy :: _ "register") unit Register.component {} (Just <<< UpdateNotifications)
          SettingsR ->
            HH.slot (SProxy :: _ "settings") unit Settings.component { currentUser } (Just <<< UpdateNotifications)
              # authorize currentUser
      , HH.div
        [ css_ "notices is-bottom" ]
        [ HH.slot
            (SProxy :: _ "notifications")
            unit NotificationMain.component { notifications }
            (Just <<< UpdateNotifications)
              ]
      ]

    Nothing ->
      HH.div_ [ HH.text "Oh no! That page wasn't found." ]
