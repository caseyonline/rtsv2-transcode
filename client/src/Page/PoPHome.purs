module Rtsv2App.Page.PoPHome where

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
import Data.Newtype (un)
import Data.Symbol (SProxy(..))
import Debug.Trace (spy, traceM)
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
import Rtsv2App.Component.HTML.Utils (css_, safeHref)
import Rtsv2App.Data.PoPDef (PoPDefEcharts, getPoPEcharts)
import Rtsv2App.Data.Profile (Profile)
import Rtsv2App.Data.Route (Route(..))
import Rtsv2App.Env (UrlEnv, UserEnv, PoPDefEnv)
import Shared.Types (PoPName(..))
import Shared.Types.Agent.State (TimedPoPRoutes, PoPDefinition)

-------------------------------------------------------------------------------
-- Types for Dashboard Page
-------------------------------------------------------------------------------
type Input =
  { popName :: PoPName
  }

data Action
  = Initialize
  | Receive Input

type State =
  { currentUser     :: Maybe Profile
  , timedRoutes     :: Maybe (Array (TimedPoPRoutes Array))
  , popDefenition   :: Maybe (PoPDefinition Array)
  , popDefEcharts   :: Array PoPDefEcharts
  , chart           :: Maybe EC.Instance
  , isOpen          :: Boolean
  , selectedRoute   :: Maybe String
  , availableRoutes :: Array String
  , popName         :: PoPName
  }

type ChildSlots =
  ( mainMenu :: MM.Slot Unit
  , header :: HD.Slot Unit
  , dropDown :: DP.Slot Unit
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
  => H.Component HH.HTML (Const Void) Input Void m
component = H.mkComponent
  { initialState
  , render
  , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      , receive = Just <<< Receive
      , initialize = Just Initialize
      }
  }
  where
  initialState { popName } =
    { currentUser: Nothing
    , timedRoutes: Nothing
    , popDefenition: Nothing
    , popDefEcharts: []
    , popName
    , selectedRoute: Just "fra"
    , isOpen: false
    , availableRoutes: ["dia", "Dal", "lax", "fra"]
    , chart: Nothing
    }

  handleAction :: Action -> H.HalogenM State Action ChildSlots Void m Unit
  handleAction = case _ of
    Initialize -> do
      st ← H.get
      { popDefEnv, urlEnv, userEnv } <- ask

      currentUser <- H.liftEffect $ Ref.read userEnv.currentUser

      H.modify_ _ { currentUser = currentUser }

      -- theme initialisation
      liftEffect $ FF.init

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
              liftEffect $ EC.setOptionPoP {} chart
              -- TODO: this needs fixing as it needs to be removed when changing page
              -- liftEffect $ EC.ressizeObserver chart

    Receive input -> do
      st <- H.get
      when (st.popName /= input.popName) do
        traceM st
        H.put $ initialState input
        -- handleAction Initialize

      -- st <- H.get
      -- when (st.popName /= popName) do
      --   H.modify_ _ { currentUser = currentUser
      --               , popName = popName
      --               }
      --   handleAction Initialize

      -- H.modify_ _ { currentUser = currentUser }

  render :: State -> H.ComponentHTML Action ChildSlots m
  render state@{ popName, currentUser } =
    HH.div
      [ css_ "main" ]
      [ HH.slot (SProxy :: _ "header") unit HD.component { currentUser, route: Login } absurd
      , HH.slot (SProxy :: _ "mainMenu") unit MM.component { currentUser, route: PoPHome popName } absurd
      , HH.slot (SProxy :: _ "menuSecondary") unit MS.component { currentUser, route: Dashboard } absurd
      , BG.component
        [ HH.li_
          [ HH.text "Admin" ]
        , HH.li_
          [ HH.text "PoP" ]
        , HH.li_
          [ HH.text $ un PoPName popName ]
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
                [ css_ "level-item is-hero-content-item is-uppercase" ]
                [ HH.div_
                  [ HH.h1
                    [ css_ "title is-spaced" ]
                    [ HH.text $ un PoPName popName ]
                  , HH.a
                    [ css_ "subtitle"
                    , safeHref $ PoPHome $ PoPName "dal" ]
                    [ HH.span_
                      [HH.text "Dal"]
                    ]
                  ]
                ]
              ]
            ]
          ]
        ]
      , HH.section
        [ css_ "section is-main-section" ]
        [ HH.div
          [ css_ "content-body" ]
          [ HH.div
            [ css_ "row" ]
            [ HH.div
              [ css_ "col-12" ]
              [ HH.div
                [ css_ "card map" ]
                mapDiv
                --, HH.slot (SProxy :: _ "dropDown") unit DP.component { items: ["dal", "lax", "dia", "fran"]
                --  , buttonLabel: "select pop"} \_ -> Nothing
              ]
            ]
          , HH.div
            [ css_ "row" ]
            [ card "Slot aggregators:" tableAgg
            , card "Stream Details:" tableStream
            ]
          , HH.div
            [ css_ "row" ]
            [ HH.div
              [ css_ "col-lg-12 col-md-12" ]
              [ HH.div
                [ css_ "card" ]
                [ HH.div
                  [ css_ "card-body" ]
                  [ HH.h5
                    [ css_ "card-title" ]
                    [ HH.text "Ingest Details" ]
                  , tableIngest
                  ]
                ]
              ]
            ]
          ]
        ]
      , footer
      ]
    where
      mapDiv =
        [ HH.div
          [ css_ "card-body dashboard-map"
          , HP.ref (H.RefLabel "mymap")
          , CSS.style do
              Geometry.height $ Size.px (toNumber 600)
              -- Geometry.width $ Size.pct (toNumber 100)
          ]
          [ HH.div
            []
            [ HH.h5
              [ css_ "card-title text-center" ]
              [ HH.text "Timed Routes" ]
            ]
          ]
        ]

      card title table =
         HH.div
          [ css_ "col-lg-6 col-md-12" ]
          [ HH.div
            [ css_ "card" ]
            [ HH.div
              [ css_ "card-body" ]
              [ HH.h5
                [ css_ "card-title" ]
                [ HH.text title ]
              , table
              -- , HH.p
              --   [ css_ "card-text"]
              --   [ HH.text "Jelly beans sugar plum cheesecake cookie oat cake soufflé.Tootsie roll bonbon liquorice tiramisu pie powder.Donut sweet roll marzipan pastry cookie cake tootsie roll oat cake cookie. Sweet roll marzipan pastry halvah. Cake bear claw sweet. Tootsie roll pie marshmallow lollipop chupa chups donut fruitcake cake.Jelly beans sugar plum cheesecake cookie oat cake soufflé. Tart lollipop carrot cake sugar plum. Marshmallow wafer tiramisu jelly beans." ]
              ]
            ]
          ]

      tableAgg =
        HH.div
        [ css_ "table-responsive"]
        [ HH.table
          [ css_ "table" ]
          [ HH.thead_
            [HH.tr_
             [ HH.th_
               [ HH.text "Slot Name"]
             , HH.th_
               [ HH.text "PoP"]
             , HH.th_
               [ HH.text "Region"]
             , HH.th_
               [ HH.text "Address"]
             ]
            ]
          , HH.tbody_
            [ HH.td_
               [ HH.text "slot1"]
             , HH.td_
               [ HH.text "fra"]
             , HH.td_
               [ HH.text "europe"]
             , HH.td_
               [ HH.text "172.16.171.5"]
             ]
          ]
        ]

      tableStream =
        HH.div
        [ css_ "table-responsive"]
        [ HH.table
          [ css_ "table" ]
          [ HH.thead_
            [ HH.tr_
             [ HH.th_
               [ HH.text "Profiles"]
             , HH.th_
               [ HH.text "Name"]
             , HH.th_
               [ HH.text "Stream Name"]
             , HH.th
               [ css_ "text-right" ]
               [ HH.text "Bitrate"]
             ]
            ]
          , HH.tbody_
            [ HH.tr_
              [ HH.td_
               [ HH.text "0"]
             , HH.td_
               [ HH.text "low"]
             , HH.td_
               [ HH.text "slot1_500"]
             , HH.td
               [ css_ "text-right" ]
               [ HH.text "500 kbps"]
             ]
            , HH.tr_
              [ HH.td_
               [ HH.text "1"]
             , HH.td_
               [ HH.text "high"]
             , HH.td_
               [ HH.text "slot1_1000"]
             , HH.td
               [ css_ "text-right" ]
               [ HH.text "1000 kbps"]
             ]

            ]

          ]
        ]

      tableIngest =
        HH.div
        [ css_ "table-responsive"]
        [ HH.table
          [ css_ "table" ]
          [ HH.thead_
            [ HH.tr_
             [ HH.th_
               [ HH.text "Profiles"]
             , HH.th_
               [ HH.text "Name"]
             , HH.th_
               [ HH.text "Audio Bitrate"]
             , HH.th_
               [ HH.text "Audio Sample Rate"]
             , HH.th_
               [ HH.text "Video Bitrate"]
             , HH.th_
               [ HH.text "Video FPS"]
             , HH.th_
               [ HH.text "Resolution"]
             ]
            ]
          , HH.tbody_
            [ HH.tr_
              [ HH.td_
               [ HH.text "0"]
             , HH.td_
               [ HH.text "low"]
             , HH.td_
               [ HH.text "32 kbps"]
             , HH.td_
               [ HH.text "44.1k"]
             , HH.td_
               [ HH.text "481 kbps"]
             , HH.td_
               [ HH.text "25"]
             , HH.td_
               [ HH.text "480 x 360"]
             ]
            , HH.tr_
              [ HH.td_
               [ HH.text "1"]
             , HH.td_
               [ HH.text "high"]
             , HH.td_
               [ HH.text "192 kbps"]
             , HH.td_
               [ HH.text "44.1k"]
             , HH.td_
               [ HH.text "701 kbps"]
             , HH.td_
               [ HH.text "25"]
             , HH.td_
               [ HH.text "1280 x 720"]
             ]
            ]
          ]
        ]
