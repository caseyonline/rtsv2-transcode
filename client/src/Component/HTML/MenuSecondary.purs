module Rtsv2App.Component.HTML.MainSecondary where

import Prelude

import Control.Monad.Reader (class MonadAsk, ask)
import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Data.Newtype (un)
import Effect.Aff.Class (class MonadAff)
import Effect.Ref as Ref
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Rtsv2App.Capability.Navigate (class Navigate, logout)
import Rtsv2App.Component.HTML.Utils (css_, safeHref)
import Rtsv2App.Data.Profile (Profile)
import Rtsv2App.Data.Route (Route(..))
import Rtsv2App.Env (PoPDefEnv)
import Shared.Types (PoPName(..), RegionName(..))
import Shared.Types.Agent.State (PoPDefinition, PoP)

data Action
  = LogUserOut
  | Receive Input

type State =
  { currentUser :: Maybe Profile
  , route :: Route
  , popDef :: Maybe (PoPDefinition Array)
  }

type Input =
  { currentUser :: Maybe Profile
  , route :: Route
  }

type Slot
  = H.Slot (Const Void) Void

component
  :: forall m r
   . MonadAff m
  => MonadAsk { popDefEnv :: PoPDefEnv | r } m
  => Navigate m
  => H.Component HH.HTML (Const Void) Input Void m
component = H.mkComponent
  { initialState: initialState
  , render
  , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      , receive = Just <<< Receive
      }
  }
  where
  initialState :: Input -> State
  initialState { currentUser, route } =
    { currentUser
    , route
    , popDef: Nothing
    }

  handleAction = case _ of
    Receive s -> do
      { popDefEnv } <- ask
      popDef <- H.liftEffect $ Ref.read popDefEnv.popDefinition
      H.put { currentUser: s.currentUser
            , route: s.route
            , popDef
            }


    LogUserOut -> logout

  render state@{ currentUser, route, popDef } =
    HH.aside
    [ css_ "aside is-placed-left is-expanded is-secondary is-hidden"
    , HP.id_ "aside-secondary"
    ]
    [ topTitle
    , HH.div
      [ css_ "menu-container jb-has-perfect-scrollbar" ]
      [ getPoPMenu route popDef ]
    , HH.div
      []
      []
    ]


-- |Top title
topTitle :: forall p i. HH.HTML p i
topTitle =
  HH.div
  [ css_ "aside-tools has-icon" ]
  [ HH.div
    [ css_ "aside-tools-label" ]
    [ HH.span
      [ css_ "icon" ]
      [ HH.i
        [ css_ "mdi mdi-server-network default" ]
        []
      ]
    , HH.span_
      [ HH.text "PoPs" ]
    ]
  , HH.a
    [ css_ "aside-close"]
    [ HH.span
      [ css_ "mdi mdi-close default" ]
      []
    ]
  ]

getPoPMenu :: forall p i. Route -> Maybe (PoPDefinition Array) -> HH.HTML p i
getPoPMenu route popDef =
   HH.div
   [ css_ "menu is-menu-main" ]
   case popDef of
     Nothing -> []
     Just pf -> (\r ->
                  HH.div_
                  [ HH.p
                   [ css_ "menu-label" ]
                   [ HH.text $ un RegionName r.name ]
                  , HH.ul
                    [ css_ "menu-list"]
                    (getPoPLi r.pops)
                  ]
                ) <$> pf.regions


getPoPLi :: forall p i.  Array (PoP Array) -> Array (HH.HTML p i)
getPoPLi pop =
  (\p ->
    HH.li
    [ css_ "aside-secondary-li"]
    [ HH.a
      [ safeHref $ PoPHome p.name ]
      [ HH.span
        [ css_ "icon" ]
        [ HH.i
          [ css_ "mdi mdi-server" ]
          []
        ]
      , HH.span
        [ css_ "menu-item-label is-upper" ]
        [ HH.text $ un PoPName p.name ]
      ]
    ]
  ) <$> pop
