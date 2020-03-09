module Rtsv2App.Component.HTML.Menu.MainSecondary where

import Prelude

import Control.Monad.Reader (class MonadAsk, ask)
import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Data.Newtype (un)
import Data.String (replace)
import Data.String.Pattern (Pattern(..), Replacement(..))
import Data.Traversable (for_)
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Rtsv2App.Capability.Navigate (class Navigate, logout)
import Rtsv2App.Component.HTML.Utils (css_, safeHref)
import Rtsv2App.Data.Profile (Profile)
import Rtsv2App.Data.Route (Route(..))
import Rtsv2App.Env (PoPDefEnv)
import Shared.Types (PoPName(..), RegionName(..))
import Shared.Types.Agent.State (PoPDefinition, PoP)
import Web.DOM.Element (className, setClassName) as DOM
import Web.DOM.NonElementParentNode (getElementById) as DOM
import Web.HTML (window) as HTML
import Web.HTML.HTMLDocument (toNonElementParentNode) as HTML
import Web.HTML.Window (document) as HTML

data Action
  = LogUserOut
  | Receive Input
  | CloseSecondaryMenu

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

    CloseSecondaryMenu -> do
      _ <- liftEffect $ closeSecondaryMenu
      pure unit


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

getPoPMenu :: forall p. Route -> Maybe (PoPDefinition Array) -> HH.HTML p Action
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


getPoPLi :: forall p.  Array (PoP Array) -> Array (HH.HTML p Action)
getPoPLi pop =
  (\p ->
    HH.li
    [ css_ "aside-secondary-li"]
    [ HH.a
      [ safeHref $ PoPDashboardR p.name
      , HE.onClick \_ -> Just CloseSecondaryMenu
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



closeSecondaryMenu :: Effect Unit
closeSecondaryMenu = do
  -- | setup parent container
  document <- HTML.window >>= HTML.document
  let parent = HTML.toNonElementParentNode document

  -- | get the elements by id
  mainContainer <- DOM.getElementById "aside-main" parent
  secondaryContainer <- DOM.getElementById "aside-secondary" parent

  -- | remove or add classes to specific elements
  for_ mainContainer $ (\cont -> removeClass "has-secondary" cont)
  for_ secondaryContainer (\cont -> addClass " is-hidden" cont)

  where
    addClass css cont = do
      mainClass <- DOM.className cont
      DOM.setClassName ( mainClass <> css ) cont

    removeClass css cont = do
      mainClass <- DOM.className cont
      let newClass = replace (Pattern css) (Replacement "") mainClass
      DOM.setClassName newClass cont
