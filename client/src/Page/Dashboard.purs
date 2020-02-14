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
import Foreign.FrontEnd as FF
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS as CSS
import Halogen.HTML.Properties as HP
import Rtsv2App.Capability.Navigate (class Navigate)
import Rtsv2App.Capability.Resource.Api (class ManageApi, getPoPdefinition, getTimedRoutes)
import Rtsv2App.Capability.Resource.User (class ManageUser)
import Rtsv2App.Component.HTML.Breadcrumb as BG
import Rtsv2App.Component.HTML.Dropdown as DP
import Rtsv2App.Component.HTML.Footer (footer)
import Rtsv2App.Component.HTML.Header as HD
import Rtsv2App.Component.HTML.MainSecondary as MS
import Rtsv2App.Component.HTML.MenuMain as MM
import Rtsv2App.Component.HTML.Tile as TL
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
  , timedRoutes     :: Maybe (Array (TimedPoPRoutes Array))
  , popDefenition   :: Maybe (PoPDefinition Array)
  , popDefEcharts   :: Array PoPDefEcharts
  , chart           :: Maybe EC.Instance
  , isOpen          :: Boolean
  , selectedRoute   :: Maybe String
  , availableRoutes :: Array String
  }

type ChildSlots =
  ( menuMain :: MM.Slot Unit
  , header :: HD.Slot Unit
  , menuSecondary :: MS.Slot Unit
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

      -- theme initialisation
      liftEffect $ FF.init

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
              liftEffect $ EC.ressizeObserver chart
              
    Receive { currentUser } ->
      H.modify_ _ { currentUser = currentUser }

  render :: State -> H.ComponentHTML Action ChildSlots m
  render state@{ currentUser } =
    HH.div
      [ HP.id_ "app" ]
      [ HH.slot (SProxy :: _ "header") unit HD.component { currentUser, route: Dashboard } absurd
      , HH.slot (SProxy :: _ "menuMain") unit MM.component { currentUser, route: Dashboard } absurd
      , HH.slot (SProxy :: _ "menuSecondary") unit MS.component { currentUser, route: Dashboard } absurd
      , BG.component
        [ HH.li_
          [ HH.text "Admin" ]
        , HH.li_
          [ HH.text "Dashboard" ]
        ]
      , HH.section
        [ css_ "hero is-hero-bar is-main-hero" ]
        [ HH.div
          [ css_ "hero-body" ]
          [ HH.div
            [ css_ "level"]
            [ HH.div
              [ css_ "level-left" ]
              [ HH.div
                [ css_ "level-item is-hero-content-item" ]
                [ HH.div_
                  [ HH.h1
                    [ css_ "title is-spaced" ]
                    [ HH.text "Network Overview" ]
                  , HH.h3
                    [ css_ "subtitle"]
                    []
                  ]
                ]
              ]
            ]
          ]
        ]
      , HH.section
        [ css_ "section is-main-section" ]
        [ HH.div
          [ css_ "tile is-ancestor"]
          [ TL.component tileStreams
          , TL.component tilePoP
          , TL.component tileWarning
          ]
        , HH.div
          [ css_ "card map" ]
          [ HH.div
            [ css_ "card-body dashboard-map"
            , HP.ref (H.RefLabel "mymap")
            , CSS.style do
              Geometry.height $ Size.px (toNumber 500)
            ]
            []
          ]
        ]
      , footer
      ]
      where
        tileWarning =
          { headerIconType: "mdi-arrow-down-bold"
          , headerText    : "25 errors on 15-02-2020"
          , subTitleH3Text: "Warnings"
          , subTitleH1Text: "5"
          , widgetTextType: "has-text-danger"
          , widgetIconType: "mdi-bell"
          }
        tileStreams =
          { headerIconType: "mdi-arrow-up-bold"
          , headerText    : "33 streams on 15-02-2020"
          , subTitleH3Text: "Active Streams"
          , subTitleH1Text: "37"
          , widgetTextType: "has-text-success"
          , widgetIconType: "mdi-video-wireless"
          }
        tilePoP =
          { headerIconType: "mdi-arrow-up-bold"
          , headerText    : "4 active PoPs on 15-02-2020"
          , subTitleH3Text: "Active PoPs"
          , subTitleH1Text: "4"
          , widgetTextType: "has-text-success"
          , widgetIconType: "mdi-server"
          }
