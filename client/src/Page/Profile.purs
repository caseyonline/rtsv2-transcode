module Rtsv2App.Page.Profile where

import Prelude

import Control.Monad.Reader (class MonadAsk, asks)
import Data.Const (Const)
import Data.Lens (Traversal')
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Ref as Ref
import Halogen as H
import Halogen.HTML as HH
import Network.RemoteData (RemoteData(..), _Success, fromMaybe, toMaybe)
import Rtsv2App.Capability.Navigate (class Navigate)
import Rtsv2App.Capability.Resource.User (class ManageUser, getAuthor)
import Rtsv2App.Component.HTML.Footer (footer)
import Rtsv2App.Component.HTML.Header as HD
import Rtsv2App.Component.HTML.MainMenu as MM
import Rtsv2App.Component.HTML.Utils (css, maybeElem)
import Rtsv2App.Data.Profile (Profile, Author)
import Rtsv2App.Data.Route (Route(..))
import Rtsv2App.Data.Username (Username)
import Rtsv2App.Data.Username as Username
import Rtsv2App.Env (UserEnv)


data Action
  = Initialize
  | Receive Input
  | LoadAuthor

type State =
  { author :: RemoteData String Author
  , currentUser :: Maybe Profile
  , username :: Username
  }

type Input =
  { username :: Username }

type ChildSlots =
  ( mainMenu :: MM.Slot Unit
  , header :: HD.Slot Unit
  )

component
  :: forall m r
   . MonadAff m
  => Navigate m
  => MonadAsk { userEnv :: UserEnv | r } m
  => ManageUser m
  => H.Component HH.HTML (Const Void) Input Void m
component = H.mkComponent
  { initialState
  , render
  , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      , receive = Just <<< Receive
      , initialize = Just Initialize
      }
  }
  where
  initialState :: Input -> State
  initialState { username } =
    { author: NotAsked
    , currentUser: Nothing
    , username
    }

  handleAction :: Action -> H.HalogenM State Action ChildSlots Void m Unit
  handleAction = case _ of
    Initialize -> do
      mbProfile <- H.liftEffect <<< Ref.read =<< asks _.userEnv.currentUser
      st <- H.modify _ { currentUser = mbProfile }
      void $ H.fork $ handleAction LoadAuthor

    Receive { username } -> do
      st <- H.get
      when (st.username /= username) do
        H.modify_ _ { username = username }
        void $ H.fork $ handleAction Initialize

    LoadAuthor -> do
      st <- H.modify _ { author = Loading }
      author <- getAuthor st.username
      H.modify_ _ { author = fromMaybe author }

  _author :: Traversal' State Author
  _author = prop (SProxy :: SProxy "author") <<< _Success

  render :: State -> H.ComponentHTML Action ChildSlots m
  render state@{ currentUser  } =
    HH.div
      [ css "main" ]
      [ HH.slot (SProxy :: _ "header") unit HD.component { currentUser, route: Register } absurd
      , HH.slot (SProxy :: _ "mainMenu") unit MM.component { currentUser , route: Register } absurd
      , HH.div
          [ css "profile-page" ]
          [ userInfo state
          , HH.div
              [ css "container" ]
              [ HH.div
                  [ css "row" ]
                  [ mainView state ]
              ]
          ]
      , footer
      ]

  userInfo state =
    HH.div
      [ css "user-info"]
      [ HH.div
          [ css "container" ]
          [ HH.div
              [ css "row" ]
              [ HH.div
                  [ css "col-xs-12 col-md-10 offset-md-1" ]
                  [ HH.img
                      [ css "user-img"
                      ]
                  , HH.h4_
                      [ HH.text $ Username.toString state.username ]
                  , maybeElem (_.bio =<< toMaybe state.author) \str ->
                      HH.p_
                        [ HH.text str ]
                  ]
              ]
          ]
      ]

  mainView state =
    HH.div
      [ css "col-xs-12 col-md-10 offset-md-1" ]
      [ HH.div
          [ css "articles-toggle" ]
          [ HH.ul
              [ css "nav nav-pills outline-active" ]
              []
          ]
      ]
