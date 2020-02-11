module Rtsv2App.Page.Dashboard where

import Prelude

import CSS.Geometry as Geometry
import CSS.Size as Size
import Component.HOC.Connect as Connect
import Control.Monad.Reader (class MonadAsk, ask)
import Data.Const (Const)
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.Symbol (SProxy(..))
import Debug.Trace (spy)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Foreign.ECharts as EC
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS as CSS
import Halogen.HTML.Properties as HP
import Rtsv2App.Capability.Navigate (class Navigate)
import Rtsv2App.Capability.Resource.Api (class ManageApi, getPoPdefinition, getTimedRoutes)
import Rtsv2App.Capability.Resource.User (class ManageUser)
import Rtsv2App.Component.HTML.Dropdown as DP
import Rtsv2App.Component.HTML.Footer (footer)
import Rtsv2App.Component.HTML.Header as HD
import Rtsv2App.Component.HTML.MainMenu as MM
import Rtsv2App.Component.HTML.Utils (css_)
import Rtsv2App.Data.PoPDef (PoPDefEcharts, getPoPEcharts)
import Rtsv2App.Data.Profile (Profile)
import Rtsv2App.Data.Route (Route(..))
import Rtsv2App.Env (UrlEnv, UserEnv, PoPDefEnv)
import Shared.Types (PoPName(..))
import Shared.Types.Agent.State (TimedPoPRoutes, PoPDefinition)

-------------------------------------------------------------------------------
-- Types for Dashboard Page
-------------------------------------------------------------------------------
data Action
  = Initialize
  | Receive { currentUser :: Maybe Profile }

type State =
  { currentUser     :: Maybe Profile
  , timedRoutes     :: Maybe (Array TimedPoPRoutes)
  , popDefenition   :: Maybe (PoPDefinition Array)
  , popDefEcharts   :: Array PoPDefEcharts
  , chart           :: Maybe EC.Instance
  , isOpen          :: Boolean
  , selectedRoute   :: Maybe String
  , availableRoutes :: Array String
  }

type ChildSlots =
  ( mainMenu :: MM.Slot Unit
  , header :: HD.Slot Unit
  , dropDown :: DP.Slot Unit
  )

-------------------------------------------------------------------------------
-- Components
-------------------------------------------------------------------------------
component
  :: forall m r
   . MonadAff m
  => MonadAsk { userEnv :: UserEnv, urlEnv :: UrlEnv, popDefEnv :: PoPDefEnv | r } m
  => Navigate m
  => ManageUser m
  => ManageApi m
  => H.Component HH.HTML (Const Void) {} Void m
component = Connect.component $ H.mkComponent
  { initialState
  , render
  , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      , receive = Just <<< Receive
      , initialize = Just Initialize
      }
  }
  where
  initialState { currentUser } =
    { currentUser
    , timedRoutes: Nothing
    , popDefenition: Nothing
    , popDefEcharts: []
    , selectedRoute: Just "fra"
    , isOpen: false
    , availableRoutes: ["dia", "Dal", "lax", "fra"]
    , chart: Nothing
    }

  handleAction :: Action -> H.HalogenM State Action ChildSlots Void m Unit
  handleAction = case _ of
    Initialize -> do
      st ← H.get

      { popDefEnv, urlEnv } <- ask
      -- curHostUrl <- urlEnv.curHostUrl
      popDef <- H.liftEffect $ Ref.read popDefEnv.popDefinition

      -- | is popDefinition already on Global
      case popDef of
        -- | no then go get it manually, update locally and globally
        Nothing -> do
          popDefenition <- getPoPdefinition
          case popDefenition of
            Left e ->  H.modify_ _ { popDefenition = Nothing }
            Right pd -> do
                -- | update global popDef
                liftEffect $ Ref.write (Just pd) popDefEnv.popDefinition
                -- | update locat state
                H.modify_ _ { popDefenition = (Just pd)
                            , popDefEcharts = getPoPEcharts pd
                            }
        -- | yes update local state
        Just pd -> H.modify_ _ { popDefenition = (Just pd)
                               , popDefEcharts = getPoPEcharts pd
                               }


      mbTimedRoutes <- getTimedRoutes $ PoPName $ fromMaybe "" st.selectedRoute
      case mbTimedRoutes of
        Left e -> H.modify_ _ { timedRoutes = Nothing }
        Right timedRoutes -> do
          newSt <- H.get
          H.getHTMLElementRef (H.RefLabel "mymap") >>= traverse_ \element -> do
              chart <- H.liftEffect $ EC.makeChart element
              H.modify_ _ { chart = Just chart }
              liftEffect $ EC.setOption { scatterData: newSt.popDefEcharts } chart
              liftEffect $ EC.setClick { curHost: (unwrap urlEnv.curHostUrl), url: "/app/?#/pop/" } chart

    Receive { currentUser } ->
      H.modify_ _ { currentUser = currentUser }

  render :: State -> H.ComponentHTML Action ChildSlots m
  render state@{ currentUser } =
    HH.div
      [ css_ "main" ]
      [ HH.slot (SProxy :: _ "header") unit HD.component { currentUser, route: Login } absurd
      , HH.slot (SProxy :: _ "mainMenu") unit MM.component { currentUser, route: Dashboard } absurd
      , HH.div
        [ css_ "app-content content" ]
        [ HH.div
          [ css_ "content-wrapper" ]
          [ HH.div
            [ css_ "content-header row" ]
            [ HH.div
              [ css_ "content-header-left col-md-4 col-12 mb-2" ]
              [ HH.h3
                [ css_ "content-header-h3" ]
                [ HH.text "Dashboard" ]
              ]
            ]
          , HH.div
            [ css_ "content-body" ]
            [ HH.div
              [ css_ "row" ]
              [ HH.div
                [ css_ "col-12" ]
                [ HH.div
                  [ css_ "card map" ]
                  html
                -- , HH.slot (SProxy :: _ "dropDown") unit DP.component { items: ["dal", "lax", "dia", "fran"]
                --                                                      , buttonLabel: "select pop"} \_ -> Nothing
                ]
              ]
            ]
          ]
        ]
      , footer
      ]
    where
      html =
        [ HH.div
          [ css_ "card-body dashboard-map"
          , HP.ref (H.RefLabel "mymap")
          , CSS.style do
              Geometry.height $ Size.px (toNumber 600)
              -- Geometry.width $ Size.pct (toNumber 100)
          ]
          []
        ]
