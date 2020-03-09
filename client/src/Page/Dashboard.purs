module Rtsv2App.Page.Dashboard where

import Prelude

import CSS.Geometry as Geometry
import CSS.Size as Size
import Component.HOC.Connect as Connect
import Control.Monad.Reader (class MonadAsk, ask)
import Data.Array (length)
import Data.Const (Const)
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Symbol (SProxy(..))
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
import Rtsv2App.Capability.Resource.Api (class ManageApi, getPoPdefinition)
import Rtsv2App.Capability.Resource.User (class ManageUser)
import Rtsv2App.Component.HTML.Breadcrumb as BG
import Rtsv2App.Component.HTML.Footer (footer)
import Rtsv2App.Component.HTML.Header as HD
import Rtsv2App.Component.HTML.Menu.MainSecondary as MS
import Rtsv2App.Component.HTML.Menu.MenuMain as MM
import Rtsv2App.Component.HTML.Tile as TL
import Rtsv2App.Component.HTML.Utils (css_)
import Rtsv2App.Data.PoP (PoPDefEcharts, updatePoPDefEnv)
import Rtsv2App.Data.Profile (Profile)
import Rtsv2App.Data.Route (Route(..))
import Rtsv2App.Env (PoPDefEnv, UrlEnv, UserEnv, changeHtmlClass)
import Shared.Types (Server)
import Shared.Types.Agent.State (PoPDefinition, AggregatorLocation)

-------------------------------------------------------------------------------
-- Types for Dashboard Page
-------------------------------------------------------------------------------
data Action
  = Initialize
  | Receive { currentUser :: Maybe Profile }

type State =
  { currentUser   :: Maybe Profile
  , popDefenition :: Maybe (PoPDefinition Array)
  , popDefEcharts :: Array PoPDefEcharts
  , chart         :: Maybe EC.Instance
  , popLeaders    :: Array Server
  , aggrLocs      :: Array (AggregatorLocation Array)
  }

type ChildSlots =
  ( menuMain      :: MM.Slot Unit
  , header        :: HD.Slot Unit
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
    , popDefenition: Nothing
    , popDefEcharts: mempty
    , chart: Nothing
    , popLeaders: mempty
    , aggrLocs: mempty
    }

  handleAction :: Action -> H.HalogenM State Action ChildSlots Void m Unit
  handleAction = case _ of
    Initialize -> do
      st ← H.get
      { popDefEnv, urlEnv } <- ask
      _ <- liftEffect $ changeHtmlClass urlEnv.htmlClass
      popDefinition <- H.liftEffect $ Ref.read popDefEnv.popDefinition
      -- init menu JS
      liftEffect $ FF.init
      -- | is popDefinition already in global state popDefEnv
      case popDefinition of
        Nothing -> do
          popDef <- getPoPdefinition
          case popDef of
            Left e ->  H.modify_ _ { popDefenition = Nothing }
            Right pd -> do
              { popDefenition, popDefEcharts, popLeaders, aggrLocs } <- updatePoPDefEnv pd
              H.modify_ _ { popDefenition = popDefenition
                          , popDefEcharts = popDefEcharts
                          , popLeaders = popLeaders
                          , aggrLocs = aggrLocs
                          }

        Just pd -> do
          { popDefenition, popDefEcharts, popLeaders, aggrLocs } <- updatePoPDefEnv pd
          H.modify_ _ { popDefenition = popDefenition
                      , popDefEcharts = popDefEcharts
                      , popLeaders = popLeaders
                      , aggrLocs = aggrLocs
                      }

      -- | set up the map and populate it with all current pops
      H.getHTMLElementRef (H.RefLabel "mymap") >>= traverse_ \element -> do
        newSt <- H.get
        chart <- H.liftEffect $ EC.makeChart element
        liftEffect $ EC.makeBlankMap chart
        H.modify_ _ { chart = Just chart }
        liftEffect $ EC.setOption { scatterData: newSt.popDefEcharts } chart
        liftEffect $ EC.setClick { curHost: (unwrap urlEnv.curHostUrl), url: "/support#/pop/" } chart
        liftEffect $ EC.ressizeObserver chart

    Receive { currentUser } ->
      H.modify_ _ { currentUser = currentUser }

  render :: State -> H.ComponentHTML Action ChildSlots m
  render state@{ currentUser, popLeaders, aggrLocs } =
    HH.div
      [ HP.id_ "app" ]
      [ HH.slot (SProxy :: _ "header") unit HD.component { currentUser, route: DashboardR } absurd
      , HH.slot (SProxy :: _ "menuMain") unit MM.component { currentUser, route: DashboardR } absurd
      , HH.slot (SProxy :: _ "menuSecondary") unit MS.component { currentUser, route: DashboardR } absurd
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
          [ TL.component (tileAggregator $ length $ aggrLocs)
          , TL.component (tilePoP $ length popLeaders)
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
          { headerIconType: "mdi-information"
          , headerText    : "Warnings"
          , subTitleH3Text: "Faults"
          , subTitleH1Text: "0"
          , widgetTextType: "has-text-danger"
          , widgetIconType: "mdi-bell"
          }
        tileAggregator agrN =
          { headerIconType: "mdi-information"
          , headerText    : "Aggregators"
          , subTitleH3Text: "Active"
          , subTitleH1Text: show agrN
          , widgetTextType: "has-text-success"
          , widgetIconType: "mdi-video-wireless"
          }
        tilePoP popsN =
          { headerIconType: "mdi-information"
          , headerText    : "PoPs"
          , subTitleH3Text: "Active"
          , subTitleH1Text: show popsN
          , widgetTextType: "has-text-success"
          , widgetIconType: "mdi-server"
          }
