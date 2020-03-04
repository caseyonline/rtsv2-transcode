module Rtsv2App.Component.HTML.PoPAggregator where

import Prelude

import Control.Monad.Reader.Trans (class MonadAsk, ask)
import Data.Array (find)
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
import Rtsv2App.Component.HTML.Utils (css_, dataAttr)
import Rtsv2App.Data.PoP (getPoPLeaderAddress)
import Rtsv2App.Env (PoPDefEnv)
import Shared.Stream (SlotId(..), SlotRole(..))
import Shared.Types (PoPName(..), PoPSelectedInfo, RegionName(..), Server(..), ServerAddress(..))
import Shared.Types.Agent.State (PoPDefinition, AggregatorLocation)


-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------
type Input =
  { popDef   :: Maybe (PoPDefinition Array)
  , aggrLocs :: AggregatorLocation Array
  }

type Slot = H.Slot Query Message

data Query a = IsOn (Boolean -> a)

data Message = SPoPInfo PoPSelectedInfo

data Action
  = Select (Maybe SlotId)
  | Receive Input

type CheckBoxState =
  { slotId     :: Maybe SlotId
  , isSelected :: Boolean
  }

type State =
  { aggrLocs     :: AggregatorLocation Array
  , popDef       :: Maybe (PoPDefinition Array)
  , checkedBoxes :: Array CheckBoxState
  }

-------------------------------------------------------------------------------
-- Components
-------------------------------------------------------------------------------
component
  :: forall m r
   . MonadAff m
  => MonadAsk { popDefEnv :: PoPDefEnv | r } m
  => Navigate m
  =>  H.Component HH.HTML Query Input Message m
component = H.mkComponent
  { initialState
  , render
  , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      , handleQuery = handleQuery
      , receive = Just <<< Receive
      }
  }
  where
  initialState :: Input -> State
  initialState { aggrLocs, popDef } =
    { checkedBoxes: initCheckBoxes aggrLocs --  myTestArgLocs
    , aggrLocs
    , popDef
    }

  handleAction :: Action -> H.HalogenM State Action () Message m Unit
  handleAction = case _ of
    Receive { aggrLocs, popDef } -> do
      H.put { checkedBoxes: initCheckBoxes aggrLocs --  myTestArgLocs
            , aggrLocs: aggrLocs
            , popDef
            }

    Select mSlotId -> do
      st <- H.get
      { popDefEnv } <- ask
      transPoPLeaders <- H.liftEffect $ Ref.read popDefEnv.transPoPLeaders

      newSt <- H.modify _ { checkedBoxes = updateSelected st.checkedBoxes mSlotId }
      let selectedSlot = whichSlotSelected newSt.checkedBoxes
          selectedPname = slotToPoP selectedSlot newSt.aggrLocs
      H.raise
        (SPoPInfo
           { selectedSlotId: selectedSlot
           , selectedPoPName: selectedPname
           , selectedAddress: getPoPLeaderAddress transPoPLeaders selectedPname
           }
        )
      pure unit

  handleQuery :: forall a. Query a -> H.HalogenM State Action () Message m (Maybe a)
  handleQuery = case _ of
    IsOn k -> do
      -- enabled <- H.gets _.enabled
      pure $ Nothing

  render :: State -> H.ComponentHTML Action () m
  render state =
    HH.div
    [ css_ "card has-table" ]
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
        , HH.text "Aggregators"
        ]
      ]
    , HH.div
        [ css_ "card-content" ]
        [ HH.div
          [ css_ "table-container" ]
          [ HH.table
            [ css_ "table is-fullwidth is-striped is-hoverable is-sortable is-fullwidth"]
            [ HH.thead_
              [ HH.tr_
                [ HH.th_
                  []
                , HH.th_
                  [ HH.text "Slot Name" ]
                , HH.th_
                  [ HH.text "PoP" ]
                , HH.th_
                  [ HH.text "Region" ]
                , HH.th_
                  [ HH.text "Address" ]
                ]
              ]
            , HH.tbody_
              (flip map state.aggrLocs
              -- (flip map myTestArgLocs
               (\argLoc ->
                 HH.tr_
                 ( argLoc.servers >>=
                   (\server -> do
                       let { address, pop, region } = un Server server
                       [ HH.td
                         [ css_ "checkbox-cell" ]
                         [ HH.label
                           [ css_ "b-checkbox checkbox"]
                           [ HH.input
                             [ HP.type_ HP.InputCheckbox
                             , HP.name $ show $ un SlotId argLoc.slotId
                             , HP.checked $ isChecked (Just argLoc.slotId) state.checkedBoxes
                             , HE.onChange $ const $ Just (Select $ Just argLoc.slotId)
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
                           [ HH.text $ show $ un SlotId argLoc.slotId ]
                         , HH.td
                           [ dataAttr "label" "PoP" ]
                           [ HH.text $ un PoPName pop ]
                         , HH.td
                           [ dataAttr "label" "Region" ]
                           [ HH.text $ un RegionName region ]
                         , HH.td
                           [ dataAttr "label" "Region" ]
                           [ HH.text $ un ServerAddress address ]
                         ]
                   )
                 )
               )
              )
            ]
          ]
        ]
    ]

-- | create a blank array of checkedBoxes using aggrLocs
initCheckBoxes :: AggregatorLocation Array -> Array CheckBoxState
initCheckBoxes aggrLocs = do
  map f aggrLocs
  where
    f = (\argLoc -> { slotId: (Just argLoc.slotId), isSelected: false })

-- | is the current checkbox already checked or not
isChecked :: Maybe SlotId -> Array CheckBoxState -> Boolean
isChecked slotId checkedBoxes = do
  let curCheckBox = find (\checkBox -> checkBox.slotId == slotId ) checkedBoxes
  case curCheckBox of
    Nothing -> false
    Just cc -> cc.isSelected

-- | make sure only one option is selected at a time
updateSelected :: Array CheckBoxState -> Maybe SlotId -> Array CheckBoxState
updateSelected checkedBoxes maybeSlotId =
  (\cb -> if maybeSlotId == cb.slotId
          then { slotId: cb.slotId , isSelected: not cb.isSelected }
          else { slotId: cb.slotId , isSelected: false }
  ) <$> checkedBoxes

-- | find which slot is selected
whichSlotSelected :: Array CheckBoxState -> Maybe SlotId
whichSlotSelected checkedBoxes = do
  let curSelected = find (\checkBox -> checkBox.isSelected == true ) checkedBoxes
  case curSelected of
    Nothing -> Nothing
    Just c  -> c.slotId

-- | find the popName given selected slot
slotToPoP :: Maybe SlotId -> AggregatorLocation Array -> Maybe PoPName
slotToPoP mSlotId arggLocs =
  case mSlotId of
    Nothing -> Nothing
    Just slotId -> do
      let matchedPoP = findMap (\argLoc -> matchServer argLoc) arggLocs
      case matchedPoP of
        Nothing -> Nothing
        Just x -> Just x
      where
        matchServer argLoc =
          findMap (\server -> do
                  let { pop, region, address } = un Server server
                  if argLoc.slotId == slotId
                    then Just pop
                    else Nothing ) argLoc.servers

myTestArgLocs :: AggregatorLocation Array
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
