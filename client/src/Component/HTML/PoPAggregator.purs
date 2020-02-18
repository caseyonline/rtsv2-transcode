module Rtsv2App.Component.HTML.PoPAggregator where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Rtsv2App.Capability.Navigate (class Navigate)
import Rtsv2App.Component.HTML.Utils (css_)
import Shared.Stream (StreamId)
import Shared.Types.Agent.State (PoPDefinition)


-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------
type Input =
  { popDef :: Maybe (PoPDefinition Array) }

type Slot = H.Slot Query Message

data Query a = IsOn (Boolean -> a)

data Message = CheckedStreamId (Maybe StreamId)

data Action = Select StreamId

type State = { checkedStreamId :: Maybe StreamId }


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
      }
  }
  where
  initialState :: Input -> State
  initialState _ = { checkedStreamId: Nothing }

  handleAction :: Action -> H.HalogenM State Action () Message m Unit
  handleAction = case _ of
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
              []
            ]
          ]
        ]
    ]


tableBody =
  HH.tr_
  []
