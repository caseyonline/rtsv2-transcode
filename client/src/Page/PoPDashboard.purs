module Rtsv2App.Page.PoPDashboard where

import Prelude

import CSS.Geometry as Geometry
import CSS.Size as Size
import Control.Monad.Reader (class MonadAsk, ask)
import Data.Array ((!!))
import Data.Const (Const)
import Data.Either (Either(..), hush)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (un)
import Data.Symbol (SProxy(..))
import Data.Traversable (traverse_)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Ref as Ref
import Foreign.ECharts as EC
import Foreign.FrontEnd as FF
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS as CSS
import Halogen.HTML.Properties as HP
import Record.Extra (sequenceRecord)
import Rtsv2App.Capability.Navigate (class Navigate)
import Rtsv2App.Capability.Resource.Api (class ManageApi, getPoPdefinition, getAggregatorDetails, getTimedRoutes)
import Rtsv2App.Capability.Resource.User (class ManageUser)
import Rtsv2App.Component.HTML.Breadcrumb as BG
import Rtsv2App.Component.HTML.Dropdown as DP
import Rtsv2App.Component.HTML.Footer (footer)
import Rtsv2App.Component.HTML.Header as HD
import Rtsv2App.Component.HTML.Menu.MainSecondary as MS
import Rtsv2App.Component.HTML.Menu.MenuMain as MM
import Rtsv2App.Component.HTML.Tables.PoPAggr as PA
import Rtsv2App.Component.HTML.Tables.PoPAggrDetails as PAD
import Rtsv2App.Component.HTML.Utils (css_)
import Rtsv2App.Data.PoP (PoPDefEcharts, timedRoutedToChartOps, timedRoutedToChartScatter, updatePoPDefEnv)
import Rtsv2App.Data.Profile (Profile)
import Rtsv2App.Data.Route (Route(..))
import Rtsv2App.Env (PoPDefEnv, UrlEnv, UserEnv, changeHtmlClass)
import Shared.Stream (SlotRole(..))
import Shared.Types (PoPName(..), Server)
import Shared.Types.Agent.State (AggregatorLocation, PoPDefinition, TimedPoPRoutes, IngestAggregator)

-------------------------------------------------------------------------------
-- Types for Dashboard Page
-------------------------------------------------------------------------------
type Input =
  { popName   :: PoPName
  , prevRoute :: Maybe Route
  }

data Action
  = Initialize
  | HandlePoPSlotAggrTable PA.Message
  | HandlePoPAggrDetails PAD.Message
  | Receive Input

type State =
  { aggrLocs          :: Array (AggregatorLocation Array)
  , chart             :: Maybe EC.Instance
  , currentUser       :: Maybe Profile
  , popDefEcharts     :: Array PoPDefEcharts
  , popDefenition     :: Maybe (PoPDefinition Array)
  , popLeaders        :: Array Server
  , curPopName        :: PoPName
  , selectedAggrIndex :: Maybe Int
  , selectedPoPName   :: Maybe PoPName
  , prevRoute         :: Maybe Route
  , timedRoutes       :: Maybe (Array (TimedPoPRoutes Array))
  , slotDetails       :: Maybe (IngestAggregator Array)
  }

type ChildSlots =
  ( mainMenu            :: MM.Slot Unit
  , header              :: HD.Slot Unit
  , dropDown            :: DP.Slot Unit
  , menuSecondary       :: MS.Slot Unit
  , popAggrTable        :: PA.Slot Unit
  , popAggrDetailsTable :: PAD.Slot Unit
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
  initialState { popName, prevRoute } =
    { aggrLocs: []
    , chart: Nothing
    , currentUser: Nothing
    , popDefEcharts: []
    , popDefenition: Nothing
    , popLeaders: []
    , curPopName: popName
    , selectedAggrIndex: Nothing
    , selectedPoPName: Nothing
    , prevRoute
    , timedRoutes: Nothing
    , slotDetails: Nothing
    }

  handleAction :: Action -> H.HalogenM State Action ChildSlots Void m Unit
  handleAction = case _ of
    Initialize -> do
      st ← H.get
      { popDefEnv, urlEnv, userEnv } <- ask
      -- set the class of <html />
      liftEffect $ changeHtmlClass urlEnv.htmlClass
      -- don't load js again if previous route was a PoPDashboard
      shouldLoadJS st.prevRoute

      mPopDef <- H.liftEffect $ Ref.read popDefEnv.popDefinition

      H.getHTMLElementRef (H.RefLabel "mymap") >>= traverse_ \element -> do
        chart <- H.liftEffect $ EC.makeChart element
        liftEffect $ EC.setOptionPoP { rttData: [], scatterData: [] } chart
        H.modify_ _ { chart = Just chart }

      -- | is popDefinition already on Global
      case mPopDef of
        -- | no then go get it manually, update states
        Nothing -> do
          popDef <- getPoPdefinition
          case popDef of
            Left e ->  H.modify_ _ { popDefenition = Nothing }
            Right pd -> updateState pd

        -- | yes update states
        -- TODO: could check if arggr already exists on global
        Just pd -> updateState pd

        where
          updateState pd = do
            env <- updatePoPDefEnv pd
            H.modify_ _ { popDefenition = env.popDefenition
                        , popDefEcharts = env.popDefEcharts
                        , popLeaders = env.popLeaders
                        , aggrLocs = env.aggrLocs
                        }

    Receive { popName, prevRoute } -> do
      st <- H.get
      when (st.curPopName /= popName) do
        H.put $ initialState { popName, prevRoute }
        handleAction Initialize

    HandlePoPSlotAggrTable (PA.SPoPAggrInfo selectedInfo) -> do
      H.modify_ _ { selectedAggrIndex = selectedInfo.selectedAggrIndex
                  , selectedPoPName = selectedInfo.selectedPoPName
                  }
      st <- H.get
      { popDefEnv } <- ask
      geoLocations <- H.liftEffect $ Ref.read popDefEnv.geoLocations

      -- | does a chart exist
      case st.chart of
        Nothing    -> pure unit -- TODO: make a new chart? though there should always be one
        Just chart ->
          -- | check all records are Justs
          case sequenceRecord selectedInfo of
            Nothing -> do
               liftEffect $ EC.setOptionPoP { rttData: [], scatterData: [] } chart
            Just { selectedAggrIndex, selectedSlotId, selectedAddress }  -> do
              let slotRole = fromMaybe Primary (_.role <$> st.aggrLocs !! selectedAggrIndex)
              mSlotDetails <- hush <$> getAggregatorDetails { slotId: selectedSlotId , slotRole, serverAddress: selectedAddress }
              H.modify_ _ {slotDetails = mSlotDetails}
              -- | go fetch timedroute and populate the chart/map with options
              maybeTimedRoutes <- getTimedRoutes selectedInfo st.curPopName
              case maybeTimedRoutes of
                Left e            -> pure unit -- TODO: display error
                Right timedRoutes -> do
                  let convertedRoutes = timedRoutedToChartOps timedRoutes geoLocations
                  let convertedScatterRoutes = timedRoutedToChartScatter timedRoutes geoLocations
                  liftEffect $ EC.setOptionPoP { rttData: convertedRoutes, scatterData: convertedScatterRoutes } chart

    HandlePoPAggrDetails _ ->
      pure unit

  render :: State -> H.ComponentHTML Action ChildSlots m
  render state@{ curPopName, currentUser, popDefenition, aggrLocs, selectedAggrIndex , slotDetails} =
    HH.div
      [ css_ "main" ]
      [ HH.slot (SProxy :: _ "header") unit HD.component { currentUser, route: LoginR } absurd
      , HH.slot (SProxy :: _ "mainMenu") unit MM.component { currentUser, route: PoPDashboardR curPopName } absurd
      , HH.slot (SProxy :: _ "menuSecondary") unit MS.component { currentUser, route: DashboardR } absurd
      , BG.component
        [ HH.li_
          [ HH.text "Admin" ]
        , HH.li_
          [ HH.text "PoP" ]
        , HH.li_
          [ HH.text $ un PoPName curPopName ]
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
                    [ HH.text $ un PoPName curPopName ]
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
          [ HH.div
            [ css_ "tile is-parent" ]
            [ HH.div
              [ css_ "card is-card-widget tile is-child" ]
              [ HH.slot
                  (SProxy :: _ "popAggrTable")
                  unit PA.component { aggrLocs, popDef: popDefenition, selectedAggrIndex }
                  (Just <<< HandlePoPSlotAggrTable)
              ]
            ]
          ,  HH.div
            [ css_ "tile is-parent" ]
            [ HH.div
              [ css_ "card map is-card-widget tile is-child" ]
              (mapDiv state)
            ]
          ]
         , HH.div
           [ css_ "tile is-ancestor" ]
           [ HH.div
             [ css_ "tile is-parent" ]
             [ HH.div
               [ css_ "card map is-card-widget tile is-child" ]
               [ HH.slot
                  (SProxy :: _ "popAggrDetailsTable")
                  unit PAD.component { slotDetails, selectedAggrDetailsIndex: Nothing, selectedAggrIndex }
                  (Just <<< HandlePoPAggrDetails)
              ]
             ]
           ]


          -- , HH.div
          --   [ css_ "row" ]
          --   [ HH.div
          --     [ css_ "col-lg-12 col-md-12" ]
          --     [ HH.div
          --       [ css_ "card" ]
          --       [ HH.div
          --         [ css_ "card-body" ]
          --         [ HH.h5
          --           [ css_ "card-title" ]
          --           [ HH.text "Ingest Details" ]
          --        -- , tableIngest
          --         ]
          --       ]
          --     ]
          --   ]
        ]
      , footer
      ]
    where
      mapDiv st =
        [ HH.header
           [ css_ "card-header" ]
           [ HH.p
             [ css_ "card-header-title" ]
             [ HH.span
               [css_ "icon"]
               [ HH.i
                 [ css_ "mdi mdi-routes-clock" ]
                 []
               ]
             , HH.span_
               [ HH.text "Routes  " ]
             ]
           , HH.p
             [ css_ "card-header-title is-left" ]
             [ HH.span
               [ css_ "is-uppercase"]
               [ case st.selectedPoPName of
                   Nothing -> HH.text ""
                   Just from -> HH.text $ (un PoPName from) <> " > " <> (un PoPName st.curPopName)
               ]
             ]
           ]
        , HH.div
          [ css_ "card-content dashboard-map"
          , HP.ref (H.RefLabel "mymap")
          , CSS.style do
              Geometry.height $ Size.px (toNumber 400)
          ]
          []
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

-- | We don't want the menu JS to load twice when we are already on a PoPDashoboard page
shouldLoadJS :: forall m. MonadEffect m => Maybe Route -> m Unit
shouldLoadJS =
  maybe (liftEffect $ FF.init) \route ->
    case route of
      PoPDashboardR _ -> pure unit
      _ -> liftEffect $ FF.init
