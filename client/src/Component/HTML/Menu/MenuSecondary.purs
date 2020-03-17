module Rtsv2App.Component.HTML.Menu.MenuSecondary where

import Prelude

import Control.Monad.Reader (class MonadAsk, ask)
import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Data.Monoid (guard)
import Data.Newtype (un)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Rtsv2App.Capability.Navigate (class Navigate, logout)
import Rtsv2App.Component.HTML.Menu.MainHelper (MenuState, closeSecondaryMenu)
import Rtsv2App.Component.HTML.Utils (css_, safeHref)
import Rtsv2App.Data.Profile (Profile)
import Rtsv2App.Data.Route (Route(..))
import Rtsv2App.Env (PoPDefEnv)
import Shared.Types (PoPName(..), RegionName(..))
import Shared.Types.Agent.State (PoPDefinition, PoP)

data Action
  = LogUserOut
  | Receive Input
  | CloseSecondaryMenu

type State =
  { currentUser  :: Maybe Profile
  , route        :: Route
  , popDef       :: Maybe (PoPDefinition Array)
  , isMenuClosed :: Boolean
  , curPopName   :: Maybe PoPName
  }

type Input =
  { currentUser  :: Maybe Profile
  , route        :: Route
  , isMenuClosed :: Boolean
  , curPopName   :: Maybe PoPName
  }

type Slot
  = H.Slot (Const Void) MenuState

component
  :: forall m r
   . MonadAff m
  => MonadAsk { popDefEnv :: PoPDefEnv | r } m
  => Navigate m
  => H.Component HH.HTML (Const Void) Input MenuState m
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
  initialState { currentUser, route, isMenuClosed, curPopName } =
    { currentUser
    , route
    , popDef: Nothing
    , isMenuClosed
    , curPopName
    }

  handleAction :: Action -> H.HalogenM State Action () MenuState m Unit
  handleAction = case _ of
    Receive s -> do
      { popDefEnv } <- ask
      popDef <- H.liftEffect $ Ref.read popDefEnv.popDefinition
      H.put { currentUser: s.currentUser
            , route: s.route
            , popDef
            , isMenuClosed: s.isMenuClosed
            , curPopName: s.curPopName
            }

    LogUserOut -> logout

    CloseSecondaryMenu -> do
      _ <- liftEffect $ closeSecondaryMenu
      pure unit


  render state@{ currentUser, route, popDef, isMenuClosed } =
    HH.aside
    [ css_ "aside is-placed-left is-expanded is-secondary is-hidden"
    , HP.id_ "aside-secondary"
    ]
    [ topTitle
    , HH.div
      [ css_ "menu-container jb-has-perfect-scrollbar" ]
      [ getPoPMenu state ]
    , HH.div
      []
      []
    ]


-- |Top title
topTitle :: forall p. HH.HTML p Action
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
    [ css_ "aside-close"
    , HE.onClick \_ -> Just CloseSecondaryMenu
    ]
    [ HH.span
      [ css_ "mdi mdi-close default" ]
      []
    ]
  ]

getPoPMenu :: forall p. State -> HH.HTML p Action
getPoPMenu state@{ route, popDef } =
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
                    (getPoPLi state r.pops)
                  ]
                ) <$> pf.regions


getPoPLi :: forall p.  State -> Array (PoP Array) -> Array (HH.HTML p Action)
getPoPLi state@{ isMenuClosed, route, curPopName } pop =
  (\p ->
    HH.li
    [ css_ "aside-secondary-li"]
    [ HH.a
      [ safeHref $ PoPDashboardR p.name
      , HE.onClick \_ -> Just CloseSecondaryMenu
      , css_ (case curPopName of
          Nothing    -> ""
          Just pName -> "" <> (guard (route == PoPDashboardR p.name) " is-active")
         )
      ]
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



