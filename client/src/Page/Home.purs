-- | The Rtsv2App homepage allows users to explore articles in several ways: in a personalized feed,
-- | by tag, or by viewing all articles.
module Rtsv2App.Page.Home where

import Prelude

import Component.HOC.Connect as Connect
import Control.Monad.Reader (class MonadAsk)
import Data.Const (Const)
import Data.Lens (Traversal')
import Data.Lens.Index (ix)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Monoid (guard)
import Data.Symbol (SProxy(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Network.RemoteData (RemoteData(..), _Success, fromMaybe)
import Rtsv2App.Api.Endpoint (ArticleParams, Pagination, noArticleParams)
import Rtsv2App.Capability.Navigate (class Navigate, logout)
import Rtsv2App.Capability.Resource.Article (class ManageArticle, getArticles, getCurrentUserFeed)
import Rtsv2App.Component.HTML.Footer (footer)
import Rtsv2App.Component.HTML.Header (header)
import Rtsv2App.Component.HTML.MainMenu (mainMenu)
import Rtsv2App.Component.HTML.Utils (css)
import Rtsv2App.Component.Part.FavoriteButton (favorite, unfavorite)
import Rtsv2App.Data.Article (ArticleWithMetadata)
import Rtsv2App.Data.PaginatedArray (PaginatedArray)
import Rtsv2App.Data.Profile (Profile)
import Rtsv2App.Data.Route (Route(..))
import Rtsv2App.Env (UserEnv)
import Web.Event.Event (preventDefault)
import Web.UIEvent.MouseEvent (MouseEvent, toEvent)

data Action
  = Initialize
  | Receive { currentUser :: Maybe Profile }
  | ShowTab Tab
  | LoadFeed Pagination
  | LoadArticles ArticleParams
  | LogUserOut
  | FavoriteArticle Int
  | UnfavoriteArticle Int
  | SelectPage Int MouseEvent

type State =
  { tags :: RemoteData String (Array String)
  , articles :: RemoteData String (PaginatedArray ArticleWithMetadata)
  , tab :: Tab
  , page :: Int
  , currentUser :: Maybe Profile
  }

data Tab
  = Feed
  | Global
  | Tag String

derive instance eqTab :: Eq Tab

tabIsTag :: Tab -> Boolean
tabIsTag (Tag _) = true
tabIsTag _ = false

component
  :: forall m r
   . MonadAff m
  => MonadAsk { userEnv :: UserEnv | r } m
  => Navigate m
  => ManageArticle m
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
    { tags: NotAsked
    , articles: NotAsked
    , tab: Global
    , currentUser
    , page: 1
    }

  handleAction :: Action -> H.HalogenM State Action () Void m Unit
  handleAction = case _ of
    Initialize -> do
      state <- H.get
      case state.currentUser of
        Nothing ->
          void $ H.fork $ handleAction $ LoadArticles noArticleParams
        profile -> do
          void $ H.fork $ handleAction $ LoadFeed { limit: Just 20, offset: Nothing }
          H.modify_ _ { tab = Feed }

    Receive { currentUser } ->
      H.modify_ _ { currentUser = currentUser }

    LoadFeed params -> do
      st <- H.modify _ { articles = Loading }
      articles <- getCurrentUserFeed params
      H.modify_ _ { articles = fromMaybe articles }

    LogUserOut -> logout

    LoadArticles params -> do
      H.modify_ _ { articles = Loading }
      articles <- getArticles params
      H.modify_ _ { articles = fromMaybe articles }

    ShowTab thisTab -> do
      st <- H.get
      when (thisTab /= st.tab) do
        H.modify_ _ { tab = thisTab }
        void $ H.fork $ handleAction case thisTab of
          Feed ->
            LoadFeed { limit: Just 20, offset: Nothing }
          Global ->
            LoadArticles (noArticleParams { limit = Just 20 })
          Tag tag ->
            LoadArticles (noArticleParams { tag = Just tag, limit = Just 20 })

    FavoriteArticle index ->
      favorite (_article index)

    UnfavoriteArticle index ->
      unfavorite (_article index)

    SelectPage index event -> do
      H.liftEffect $ preventDefault $ toEvent event
      st <- H.modify _ { page = index }
      let offset = Just (index * 20)
      void $ H.fork $ handleAction case st.tab of
        Feed ->
          LoadFeed { limit: Just 20, offset }
        Global ->
          LoadArticles (noArticleParams { limit = Just 20, offset = offset })
        Tag tag ->
          LoadArticles (noArticleParams { tag = Just tag, limit = Just 20, offset = offset })

  _article :: Int -> Traversal' State ArticleWithMetadata
  _article i =
    prop (SProxy :: SProxy "articles")
      <<< _Success
      <<< prop (SProxy :: SProxy "body")
      <<< ix i

  render :: State -> H.ComponentHTML Action () m
  render state@{ tags, articles, currentUser } =
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

  tab :: forall props. State -> Tab -> HH.HTML props Action
  tab st thisTab =
    HH.li
      [ css "nav-item" ]
      [ HH.a
          [ css $ "nav-link" <> guard (st.tab == thisTab) " active"
          , HE.onClick \_ -> Just $ ShowTab thisTab
          , HP.href "#/"
          ]
          htmlBody
      ]
    where
    htmlBody = case thisTab of
      Feed ->
        [ HH.text "Your Feed" ]
      Global ->
        [ HH.text "Global Feed" ]
      Tag tag ->
        [ HH.i
          [ css "ion-pound" ]
          []
        , HH.text $ "#" <> tag
        ]
