module Rtsv2App.Component.HTML.Tables.PoPAggr where

import Prelude

import Control.Monad.Reader.Trans (class MonadAsk)
import Data.Array (mapWithIndex, null, (!!))
import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Data.Newtype (un)
import Data.String (take)
import Data.Time.Duration (Milliseconds(..))
import Effect.Aff (delay)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Foreign.ClipBoard (copyToClipboard)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Rtsv2App.Capability.Navigate (class Navigate)
import Rtsv2App.Component.HTML.Utils (css_, dataAttr)
import Rtsv2App.Component.Utils (Notification(..), NotificationContent, NotificationMessage(..), PoPAggrSelectedInfo)
import Rtsv2App.Env (PoPDefEnv)
import Shared.Stream (SlotId(..), SlotRole(..))
import Shared.Types (PoPName(..), RegionName(..), Server(..), ServerAddress(..))
import Shared.Types.Agent.State (AggregatorLocation, PoPDefinition)
import Shared.UUID as UUID

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------
type Input =
  { popDef            :: Maybe (PoPDefinition Array)
  , aggrLocs          :: Array (AggregatorLocation Array)
  , selectedAggrIndex :: Maybe Int
  }

type Slot = H.Slot (Const Void) Message

data Message
  = PoPAggrMsg PoPAggrSelectedInfo
  | CopyMsg NotificationMessage
  | RefreshAggr

data Action
  = Select Int
  | Receive Input
  | Copy String
  | Refresh

type State =
  { aggrLocs          :: Array (AggregatorLocation Array)
  , popDef            :: Maybe (PoPDefinition Array)
  , selectedAggrIndex :: Maybe Int
  , isLoading         :: Boolean
  }

-------------------------------------------------------------------------------
-- Components
-------------------------------------------------------------------------------
component
  :: forall m r
   . MonadAff m
  => MonadAsk { popDefEnv :: PoPDefEnv | r } m
  => Navigate m
  =>  H.Component HH.HTML (Const Void) Input Message m
component = H.mkComponent
  { initialState
  , render
  , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      , receive = Just <<< Receive
      }
  }
  where
  initialState :: Input -> State
  initialState { aggrLocs, popDef } =
    { aggrLocs
    , selectedAggrIndex: Nothing
    , popDef
    , isLoading: false
    }

  handleAction :: Action -> H.HalogenM State Action () Message m Unit
  handleAction = case _ of
    Receive input@{ aggrLocs, popDef, selectedAggrIndex } -> do
      -- little delay to allow loading animation to play
      _ <- liftAff $ delay $ Milliseconds 500.0
      H.modify_ _ { selectedAggrIndex = selectedAggrIndex
                  , aggrLocs = aggrLocs
                  , popDef = popDef
                  , isLoading = false
                  }

    Select index -> do
      st <- H.get
      let newSelected =
            if st.selectedAggrIndex == Just index
              then Nothing
              else Just index

      newSt <- H.modify _ { selectedAggrIndex = newSelected }
      let
        maybeAgg :: Maybe (AggregatorLocation Array)
        maybeAgg = const (newSt.aggrLocs !! index) =<< newSelected

        selectedSlot :: Maybe SlotId
        selectedSlot = _.slotId <$> maybeAgg

        -- this assumes there is only one value in servers which `might` not be the case
        firstAggregator = (\aggr -> un Server <$> aggr.servers !! 0) =<< maybeAgg

        selectedPname :: Maybe PoPName
        selectedPname = _.pop <$> firstAggregator

        selectedServer :: Maybe ServerAddress
        selectedServer = _.address <$> firstAggregator

      H.raise
        (PoPAggrMsg
           { selectedSlotId: selectedSlot
           , selectedPoPName: selectedPname
           , selectedAddress: selectedServer
           , selectedAggrIndex: newSelected
           }
        )

    Refresh -> do
      H.modify_ _ { isLoading = true }
      H.raise (RefreshAggr)

    Copy val -> do
      liftEffect $ copyToClipboard val
      H.raise (CopyMsg $ NSingleMessage $ SuccessN { message: val
                                  , autoClose: true
                                  , title: "Copied to clipboard"
                                  })

  render :: State -> H.ComponentHTML Action () m
  render state =
    HH.div
    [ css_ "has-table" ]
    [ HH.header
      [ css_ "card-header" ]
      [ HH.p
        [ css_ "card-header-title" ]
        [ HH.span
          [css_ "icon"]
          [ HH.i
            [ css_ "mdi mdi-video-wireless" ]
            []
          ]
        , HH.text "Active Aggregators"
        ]
      , HH.button
        [ css_ $ "button is-small " <> if state.isLoading then "is-loading" else ""
        , HE.onClick $ const $ Just Refresh
        ]
        [ HH.span
          [ css_ "icon" ]
          [ HH.i
            [ css_ "mdi mdi-refresh default" ]
            []
          ]
        , HH.span_
          [ HH.text "Refresh" ]
        ]
      ]
    , HH.div
        [ css_ "card-content" ]
        [ HH.div
          [ css_ "table-container" ]
          [ HH.table
            [ css_ "table is-striped is-hoverable is-sortable is-fullwidth"]
            [ HH.thead_
              [ HH.tr_
                [ HH.th
                  [ css_ "checkbox-cell title is-7" ]
                  []
                , HH.th
                  [ css_ "title is-7" ]
                  [ HH.text "Slot name" ]
                , HH.th
                  [ css_ "title is-7" ]
                  [ HH.text "Slot role" ]
                , HH.th
                  [ css_ "title is-7" ]
                  [ HH.text "PoP" ]
                , HH.th
                  [ css_ "title is-7" ]
                  [ HH.text "Region" ]
                , HH.th
                  [ css_ "title is-7" ]
                  [ HH.text "Address" ]
                ]
              ]
            , tableTRs state
            ]
          ]
        , HH.div
          [ css_ "columns"]
          ( if null state.aggrLocs then tableNoAggr else [] )
        ]
    ]


-------------------------------------------------------------------------------
-- Table content
-------------------------------------------------------------------------------
tableNoAggr :: forall p i. Array (HH.HTML p i)
tableNoAggr =
  [ HH.div
    [ css_ "column is-10 is-offset-2" ]
    [ HH.p
      [ css_ "card-header-title" ]
      [ HH.span
        [ css_ "icon is-large" ]
        [ HH.i
          [ css_ "mdi mdi-video-off-outline mdi-48px" ]
          []
        ]
      , HH.span
        [ css_ "subtitle" ]
        [ HH.text "No aggregators running" ]
      ]
    ]
  ]


-- | 
tableTRs :: forall p. State -> HH.HTML p Action
tableTRs state =
  HH.tbody_
  (flip mapWithIndex state.aggrLocs
    (\index aggrLoc ->
        HH.tr
        [ css_ if state.selectedAggrIndex == Just index then "is-selected" else "" ]
        ( aggrLoc.servers >>=
          (\server -> do
              let s = un Server server
              [ HH.td
                [ css_ "checkbox-cell" ]
                [ HH.label
                  [ css_ "b-checkbox checkbox"]
                  [ HH.input
                    [ HP.type_ HP.InputCheckbox
                    , HP.name $ show $ un SlotId aggrLoc.slotId
                    , HP.checked $ state.selectedAggrIndex == Just index
                    , HE.onChange $ const $ Just (Select index)
                    ]
                  , HH.span
                    [ css_ "check"]
                    []
                  , HH.span
                    [ css_ "control-label"]
                    []
                  ]
                ]
                , HH.td
                  [ dataAttr "label" "Name" ]
                  [ HH.text $ (take 3 $ show $ un SlotId aggrLoc.slotId) <> "..." <> " "
                  , HH.button
                    [ css_ "button is-small is-info"
                    , HE.onClick $ const $ Just $ Copy (show $ un SlotId aggrLoc.slotId)
                    ]
                    [ HH.span
                      [ css_ "icon" ]
                      [ HH.i
                        [ css_ "mdi mdi-content-copy" ]
                        []
                      ]
                    ]
                  ]
                , HH.td
                  [ dataAttr "label" "Name" ]
                  [ HH.text $ show $ aggrLoc.role ]
                , HH.td
                  [ dataAttr "label" "PoP" ]
                  [ HH.text $ un PoPName s.pop ]
                , HH.td
                  [ dataAttr "label" "Region" ]
                  [ HH.text $ un RegionName s.region ]
                , HH.td
                  [ dataAttr "label" "Region" ]
                  [ HH.text $ un ServerAddress s.address ]
                ]
          )
        )
    )
  )


---------------------------------------------------------------------------------------
-- Test Aggrs -- this is for testing purposes only when displaying multiple aggregators
---------------------------------------------------------------------------------------
-- myTestArgLocs :: Array (AggregatorLocation Array)
-- myTestArgLocs =
--   [ { slotId: SlotId $ UUID.fromString "1"
--     , role: Primary
--     , servers: [ Server
--                    { address: ServerAddress "172.16.171.1"
--                    , pop: PoPName "fra"
--                    , region: RegionName "europe"
--                    }
--                ]
--     }
--   , { slotId: SlotId $ UUID.fromString "2"
--     , role: Primary
--     , servers: [ Server
--                    { address: ServerAddress "172.16.170.1"
--                    , pop: PoPName "dal"
--                    , region: RegionName "america"
--                    }
--                ]
--     }

--   ]
