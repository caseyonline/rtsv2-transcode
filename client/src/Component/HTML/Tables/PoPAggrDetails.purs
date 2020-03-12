module Rtsv2App.Component.HTML.Tables.PoPAggrDetails where

import Prelude

import Control.Monad.Reader.Trans (class MonadAsk)
import Data.Array (elem, find, mapWithIndex)
import Data.Const (Const)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (un)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Rtsv2App.Capability.Navigate (class Navigate)
import Rtsv2App.Component.Utils (PoPAggrSelectedInfo)
import Rtsv2App.Component.HTML.Utils (css_, dataAttr)
import Rtsv2App.Env (PoPDefEnv)
import Shared.LlnwApiTypes (SlotProfile(..))
import Shared.Rtsv2.JsonLd (unwrapNode)
import Shared.Stream (ProfileName(..), RtmpStreamName(..))
import Shared.Types.Agent.State (IngestAggregator)

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------
type Input =
  { slotDetails              :: Maybe (IngestAggregator Array)
  , selectedAggrDetailsIndex :: Maybe Int
  , selectedAggrIndex        :: Maybe Int
  }

type Slot = H.Slot (Const Void) Message

data Message = SPoPInfo PoPAggrSelectedInfo -- TODO : what are we returning

data Action
  = Select Int
  | Receive Input

type State =
  { slotDetails              :: Maybe (IngestAggregator Array)
  , selectedAggrDetailsIndex :: Maybe Int
  , selectedAggrIndex        :: Maybe Int
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
  initialState { slotDetails, selectedAggrIndex } =
    { slotDetails
    , selectedAggrDetailsIndex: Nothing
    , selectedAggrIndex
    }

  handleAction :: Action -> H.HalogenM State Action () Message m Unit
  handleAction = case _ of
    Receive input@{ slotDetails, selectedAggrIndex } ->
      H.modify_ _ { slotDetails = maybe Nothing (const slotDetails) selectedAggrIndex
                  , selectedAggrIndex = selectedAggrIndex
                  }

    Select index -> do
      st <- H.get
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
        , HH.text "Aggregators Slot Details"
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
                  [ css_ "title is-7" ]
                  []
                , HH.th
                  [ css_ "title is-7" ]
                  [ HH.text "Profile name" ]
                , HH.th
                  [ css_ "title is-7" ]
                  [ HH.text "Stream name" ]
                , HH.th
                  [ css_ "title is-7" ]
                  [ HH.text "Bitrate" ]
                , HH.th
                  [ css_ "title is-7" ]
                  [ HH.text "Active" ]
                ]
              ]
            , ( maybe (HH.div_ []) (tableTRs state) state.slotDetails)
            ]
          ]
        , HH.div
          [ css_ "columns"]
          ( maybe tableNoAggr (const []) state.selectedAggrIndex)
        ]
    ]


tableTRs :: forall p. State -> IngestAggregator Array -> HH.HTML p Action
tableTRs state ingestAggr = do
  let profiles = (un SlotProfile <$> (unwrapNode ingestAggr).streamDetails.slot.profiles)
  HH.tbody_
    (flip mapWithIndex profiles
      (\index profile@{ name, rtmpStreamName, bitrate } -> do
        let activeRes = isActiveProfile name ingestAggr
        HH.tr
          [ css_ if activeRes
                  then ""
                  else "inactive"
          ]
          [ if activeRes
              then HH.td          
                  [ css_ "checkbox-cell" ]
                  [ HH.label
                    [ css_ "b-checkbox checkbox"]
                    [ HH.input
                      [ HP.type_ HP.InputCheckbox
                      , HP.checked $ state.selectedAggrDetailsIndex == Just index
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
              else HH.td_ []

          , HH.td
            [ dataAttr "label" "Profile name" ]
            [ HH.text $ un ProfileName name ]
          , HH.td
            [ dataAttr "label" "Stream name" ]
            [ HH.text $ un RtmpStreamName rtmpStreamName 
            ]
          , HH.td
            [ dataAttr "label" "Bitrate" ]
            [ HH.text $ (show bitrate) <> " bps"
            ]
          , HH.td
            [ dataAttr "label" "Active" ]
            [ HH.span
              [css_ "icon"]
              [ HH.text $ if activeRes
                            then "yes"
                            else "no"
              ]
            ]
          ]
      )
    )
    
    


tableNoAggr :: forall p i. Array (HH.HTML p i)
tableNoAggr =
  [ HH.div
    [ css_ "column is-4 is-offset-4" ]
    [ HH.p
      [ css_ "card-header-title" ]
      [ HH.span
        [ css_ "icon icon is-large" ]
        [ HH.i
          [ css_ "mdi mdi-message-alert-outline mdi-48px" ]
          []
        ]
      , HH.span
        [ css_ "subtitle" ]
        [ HH.text "No aggregator selected" ]
      ]
    ]
  ]

isActiveProfile :: ProfileName -> IngestAggregator Array -> Boolean
isActiveProfile pName ingestAggr = do
  let activeProfiles = unwrapNode <$> (unwrapNode ingestAggr).activeProfiles 
      results = (\p -> p.profileName == pName ) <$> activeProfiles
  elem true results

getSlotProfile
  :: IngestAggregator Array
  -> ProfileName
  -> Maybe { bitrate :: Int
           , name :: ProfileName
           , rtmpStreamName :: RtmpStreamName
           }

getSlotProfile inAgg pn = do
  maybe Nothing (Just <<< un SlotProfile) $ findSlotProfile inAgg pn

findSlotProfile :: IngestAggregator Array -> ProfileName -> Maybe SlotProfile
findSlotProfile ingestAggr pName =
  find (\slotProfile -> do
           let sp = un SlotProfile slotProfile
           sp.name == pName )
       (unwrapNode ingestAggr).streamDetails.slot.profiles
