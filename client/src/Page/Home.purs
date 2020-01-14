-- | The Rtsv2App homepage allows users to explore articles in several ways: in a personalized feed,
-- | by tag, or by viewing all articles.
module Rtsv2App.Page.Home where

import Prelude

import Component.HOC.Connect as Connect
import Control.Monad.Reader (class MonadAsk)
import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Rtsv2App.Capability.Navigate (class Navigate)
import Rtsv2App.Component.HTML.Footer (footer)
import Rtsv2App.Component.HTML.Header (header)
import Rtsv2App.Component.HTML.MainMenu (mainMenu)
import Rtsv2App.Component.HTML.Utils (css)
import Rtsv2App.Data.Profile (Profile)
import Rtsv2App.Data.Route (Route(..))
import Rtsv2App.Env (UserEnv)

data Action
  = Initialize
  | Receive { currentUser :: Maybe Profile }
  | LogUserOut

type State =
  { page :: Int
  , currentUser :: Maybe Profile
  }

component
  :: forall m r
   . MonadAff m
  => MonadAsk { userEnv :: UserEnv | r } m
  => Navigate m
  => H.Component HH.HTML (Const Void) {} Void m
component = Connect.component $ H.mkComponent
  { initialState
  , render
  , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      , receive = Just <<< Receive
      , initialize = Just Initialize
      }
  }
  where
  initialState { currentUser } =
    { currentUser
    , page: 1
    }
    
  handleAction :: Action -> H.HalogenM State Action () Void m Unit
  handleAction = case _ of
    Initialize -> do
      state <- H.get
      case state.currentUser of
        Nothing -> pure unit
        profile -> pure unit

    Receive { currentUser } ->
      H.modify_ _ { currentUser = currentUser }

    LogUserOut -> pure unit

  render :: State -> H.ComponentHTML Action () m
  render state@{ currentUser } =
    HH.div_
    [ header currentUser Home
    , mainMenu currentUser Home
    , HH.div
      [ css "app-content content home-page" ]
      [ HH.div
        [ css "content-wrapper" ]
        [ HH.div
          [ css "content-header row" ]
          []
        , HH.div
          [ css "content-body" ]
          [ HH.div
            [ css "container page" ]
            [ HH.div
              [ css "row" ]
              [ HH.div
                [ css "col-md-3" ]
                [ HH.div
                  [ css "sidebar" ]
                  [ HH.p_
                    [ HH.text "Popular Tags" ]
                  ]
                ]
              ]
            ]
          ]
        ]
      ]
    , footer
    ]
