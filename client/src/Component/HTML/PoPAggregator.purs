module Rtsv2App.Component.HTML.PoPAggregator where

import Prelude

import Data.Array (head)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (un)
import Debug.Trace (spy)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Rtsv2App.Capability.Navigate (class Navigate)
import Rtsv2App.Component.HTML.Utils (css_, dataAttr)
import Shared.Stream (StreamId(..))
import Shared.Types (PoPName(..), RegionName(..), Server(..), ServerAddress(..))
import Shared.Types.Agent.State (PoPDefinition, AggregatorLocation)


-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------
type Input =
  { popDef  :: Maybe (PoPDefinition Array)
  , argLocs :: AggregatorLocation Array
  }

type Slot = H.Slot Query Message

data Query a = IsOn (Boolean -> a)

data Message = CheckedStreamId (Maybe StreamId)

data Action
  = Select StreamId
  | Receive Input

type State =
  { checkedStreamId :: Maybe StreamId
  , argLocs         :: AggregatorLocation Array
  , popDef          :: Maybe (PoPDefinition Array)
  }


-------------------------------------------------------------------------------
-- Components
-------------------------------------------------------------------------------
component
  :: forall m
   . MonadAff m
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
  initialState { argLocs, popDef } =
    { checkedStreamId: Nothing
    , argLocs: (spy "poop" argLocs)
    , popDef
    }

  handleAction :: Action -> H.HalogenM State Action () Message m Unit
  handleAction = case _ of
    Receive { argLocs, popDef } -> do
      H.put { checkedStreamId: Nothing
            , argLocs: spy "Recieve" argLocs
            , popDef
            }

    Select streamId -> do
      newState <- H.modify _ { checkedStreamId = Just streamId }
      H.raise (CheckedStreamId newState.checkedStreamId)

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
              (tableBody <$> state.argLocs)
            ]
          ]
        ]
    ]

tableBody
  :: forall p i
  .  { streamId :: StreamId , servers :: Array Server}
  -> HH.HTML p i
tableBody argLoc =
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
            , -- HE.onValueInput (Just <<< Select argLoc.streamId)
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
        [ HH.text $ un StreamId argLoc.streamId ]
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



  -- ( flip map argLoc.servers
  --   (\server ->
  --     [ HH.td
  --       [ css_ "checkbox-cell" ]
  --       [ HH.label
  --         [ css_ "b-checkbox checkbox"]
  --         [ HH.input
  --           [ HP.type_ HP.InputCheckbox ]
  --         , HH.span
  --           [ css_ "check"]
  --           []
  --         , HH.span
  --           [ css_ "control-label"]
  --           []
  --         ]
  --       ]
  --     , HH.td
  --       [ dataAttr "label" "Name" ]
  --       [ -- HH.text $ un StreamId argLoc.streamId
  --       ]
  --     , HH.td
  --       [ dataAttr "label" "PoP" ]
  --       [ -- HH.text $ un PoPName server.pop
  --       ]
  --     ]
  --   )
  -- -- )
