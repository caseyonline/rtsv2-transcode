module Rtsv2App.Component.HTML.Tables.PoPAggr where

import Prelude

import Control.Monad.Reader.Trans (class MonadAsk, ask)
import Data.Array (mapWithIndex, null, (!!))
import Data.Const (Const)
import Data.Foldable (findMap)
import Data.Maybe (Maybe(..))
import Data.Newtype (un)
import Effect.Aff.Class (class MonadAff)
import Effect.Ref as Ref
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Rtsv2App.Capability.Navigate (class Navigate)
import Rtsv2App.Capability.Resource.Types (PoPAggrSelectedInfo)
import Rtsv2App.Component.HTML.Utils (css_, dataAttr)
import Rtsv2App.Env (PoPDefEnv)
import Shared.Stream (SlotId(..), SlotRole(..))
import Shared.Types (PoPName(..), RegionName(..), Server(..), ServerAddress(..))
import Shared.Types.Agent.State (AggregatorLocation, PoPDefinition)

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------
type Input =
  { popDef       :: Maybe (PoPDefinition Array)
  , aggrLocs     :: Array (AggregatorLocation Array)
  , selectedAggrIndex  :: Maybe Int
  }

type Slot = H.Slot (Const Void) Message

data Message = SPoPAggrInfo PoPAggrSelectedInfo

data Action
  = Select Int
  | Receive Input

type State =
  { aggrLocs          :: Array (AggregatorLocation Array)
  , popDef            :: Maybe (PoPDefinition Array)
  , selectedAggrIndex :: Maybe Int
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
    }

  handleAction :: Action -> H.HalogenM State Action () Message m Unit
  handleAction = case _ of
    Receive input@{ aggrLocs, popDef, selectedAggrIndex } ->
      H.modify_ _ { selectedAggrIndex = selectedAggrIndex
                  , aggrLocs = aggrLocs
                  , popDef = popDef
                  }

    Select index -> do
      st <- H.get
      let newSelected = if st.selectedAggrIndex == Just index
                then Nothing
                else Just index
      { popDefEnv } <- ask
      transPoPLeaders <- H.liftEffect $ Ref.read popDefEnv.transPoPLeaders

      newSt <- H.modify _ { selectedAggrIndex = newSelected }
      let
        maybeAgg :: Maybe (AggregatorLocation Array)
        maybeAgg = const (newSt.aggrLocs !! index) =<< newSelected
        selectedSlot = _.slotId <$> maybeAgg
        firstAggregator = (\aggr -> un Server <$> aggr.servers !! 0) =<< maybeAgg
        selectedPname = _.pop <$> firstAggregator
        selectedServer = _.address <$> firstAggregator
      H.raise
        (SPoPAggrInfo
           { selectedSlotId: selectedSlot
           , selectedPoPName: selectedPname
           , selectedAddress: selectedServer
           , selectedAggrIndex: newSelected
           }
        )
      pure unit

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



tableNoAggr :: forall p i. Array (HH.HTML p i)
tableNoAggr =
  [ HH.div
    [ css_ "column is-10 is-offset-2" ]
    [ HH.p
      [ css_ "card-header-title" ]
      [ HH.span
        [ css_ "icon icon is-large" ]
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


tableTRs :: forall p. State -> HH.HTML p Action
tableTRs state =
  HH.tbody_
  (flip mapWithIndex state.aggrLocs
   -- (flip mapWithIndex  myTestArgLocs
    (\index aggrLoc ->
        HH.tr_
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
                  [ HH.text $ show $ un SlotId aggrLoc.slotId ]
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





-- | find the popName given selected slot
slotToPoP :: Maybe SlotId -> Array (AggregatorLocation Array) -> Maybe PoPName
slotToPoP mSlotId aggrLocs =
  case mSlotId of
    Nothing -> Nothing
    Just slotId -> do
      let matchedPoP = findMap (\aggrLoc -> matchServer aggrLoc) aggrLocs
      case matchedPoP of
        Nothing -> Nothing
        Just x -> Just x
      where
        matchServer aggrLoc =
          findMap (\server -> do
                  let { pop, region, address } = un Server server
                  if aggrLoc.slotId == slotId
                    then Just pop
                    else Nothing ) aggrLoc.servers


-- | this is for testing purposes only when displaying multiple aggregators
myTestArgLocs :: Array (AggregatorLocation Array)
myTestArgLocs =
  [ { slotId: SlotId 1
    , role: Primary
    , servers: [ Server
                   { address: ServerAddress "172.16.171.1"
                   , pop: PoPName "fra"
                   , region: RegionName "europe"
                   }
               ]
    }
  , { slotId: SlotId 2
    , role: Primary
    , servers: [ Server
                   { address: ServerAddress "172.16.170.1"
                   , pop: PoPName "dal"
                   , region: RegionName "america"
                   }
               ]
    }

  ]
