module Rtsv2App.Page.ViewArticle where

import Prelude

import Control.Monad.Reader (class MonadAsk, asks)
import Data.Const (Const)
import Data.Foldable (for_)
import Data.Lens (Traversal', preview)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Data.Symbol (SProxy(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Ref as Ref
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Network.RemoteData (RemoteData(..), _Success, fromMaybe)
import Rtsv2App.Capability.Navigate (class Navigate, navigate)
import Rtsv2App.Capability.Resource.Article (class ManageArticle, deleteArticle, getArticle)
import Rtsv2App.Capability.Resource.User (class ManageUser)
import Rtsv2App.Component.HTML.Footer (footer)
import Rtsv2App.Component.HTML.Header (header)
import Rtsv2App.Component.HTML.Utils (css, maybeElem, safeHref)
import Rtsv2App.Component.Part.FavoriteButton (ButtonSize(..), favorite, favoriteButton, unfavorite)
import Rtsv2App.Component.Part.FollowButton (follow, followButton, unfollow)
import Rtsv2App.Component.RawHTML as RawHTML
import Rtsv2App.Data.Article (ArticleWithMetadata)
import Rtsv2App.Data.Avatar as Avatar
import Rtsv2App.Data.PreciseDateTime as PDT
import Rtsv2App.Data.Profile (Author, Profile)
import Rtsv2App.Data.Route (Route(..))
import Rtsv2App.Data.Username as Username
import Rtsv2App.Env (UserEnv)
import Slug (Slug)

data Action
  = Initialize
  | GetArticle
  | FollowAuthor
  | UnfollowAuthor
  | FavoriteArticle
  | UnfavoriteArticle
  | DeleteArticle

type State =
  { article :: RemoteData String ArticleWithMetadata
  , slug :: Slug
  , currentUser :: Maybe Profile
  }

type Input =
  { slug :: Slug
  }

type ChildSlots =
  ( rawHtml :: H.Slot (Const Void) Void Unit )

component
  :: forall m r
   . MonadAff m
  => ManageArticle m
  => ManageUser m
  => MonadAsk { userEnv :: UserEnv | r } m
  => Navigate m
  => H.Component HH.HTML (Const Void) Input Void m
component = H.mkComponent
  { initialState
  , render
  , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      , initialize = Just Initialize
      }
  }
  where
  initialState :: Input -> State
  initialState { slug } =
    { article: NotAsked
    , currentUser: Nothing
    , slug
    }

  handleAction :: Action -> H.HalogenM State Action ChildSlots Void m Unit
  handleAction = case _ of
    Initialize -> do
      mbProfile <- H.liftEffect <<< Ref.read =<< asks _.userEnv.currentUser
      H.modify_ _ { currentUser = mbProfile }

    GetArticle -> do
      st <- H.modify _ { article = Loading }
      article <- getArticle st.slug
      H.modify_ _ { article = fromMaybe article }

    FollowAuthor ->
      follow _author

    UnfollowAuthor ->
      unfollow _author

    FavoriteArticle ->
      favorite _article

    UnfavoriteArticle ->
      unfavorite _article

    DeleteArticle -> do
      st <- H.get
      for_ (preview _Success st.article) (deleteArticle <<< _.slug)
      navigate Home

  _author :: Traversal' State Author
  _author = _article <<< prop (SProxy :: SProxy "author")

  _article :: Traversal' State ArticleWithMetadata
  _article = prop (SProxy :: SProxy "article") <<< _Success

  render :: State -> H.ComponentHTML Action ChildSlots m
  render state =
    HH.div
      [ css "article-page" ]
      [ header state.currentUser (ViewArticle state.slug)
      , maybeElem mbArticle banner
      , maybeElem mbArticle content
      , footer
      ]
    where
    mbArticle = preview _Success state.article
    markdown = Maybe.fromMaybe "Failed to load article!" (_.body <$> mbArticle)

    banner article =
      HH.div
        [ css "banner"]
        [ HH.div
            [ css "container" ]
            [ HH.h1_
                [ HH.text article.title ]
            , articleMeta article
            ]
        ]

    content article =
      HH.div
        [ css "container page" ]
        [ HH.div
            [ css "col-xs-12" ]
            [ HH.slot (SProxy :: _ "rawHtml") unit RawHTML.component { markdown } absurd
            , HH.ul
                [ css "tag-list" ]
                (renderTag <$> article.tagList)
            , HH.hr_
            , HH.div
                [ css "article-actions" ]
                [ articleMeta article ]
            , HH.div
                [ css "row" ]
                [ HH.div
                    [ css "col-xs-12 col-md-8 offset-md-2" ]
                    []
                ]
            ]
        ]
      where
      renderTag str =
        HH.li
          [ css "tag-default tag-pill tag-outline" ]
          [ HH.text str ]

    articleMeta article =
      HH.div
        [ css "article-meta" ]
        [ HH.a
            [ safeHref $ Profile username ]
            [ HH.img
              [ HP.src $ Avatar.toStringWithDefault avatar ]
            ]
        , HH.div
            [ css "info" ]
            [ HH.a
                [ css "author"
                , safeHref $ Profile username
                ]
                [ HH.text $ Username.toString username ]
            , HH.span
                [ css "date" ]
                [ HH.text $ PDT.toDisplayMonthDayYear article.createdAt ]
            ]
        , case state.currentUser of
            Just profile | profile.username == username ->
              HH.span_
                [ HH.a
                    [ css "btn btn-outline-secondary btn-sm"
                    , safeHref $ EditArticle article.slug
                    ]
                    [ HH.i
                        [ css "ion-edit" ]
                        []
                    , HH.text " Edit Article"
                    ]
                , HH.text " "
                , HH.button
                    [ css "btn btn-outline-danger btn-sm"
                    , HE.onClick \_ -> Just DeleteArticle
                    ]
                    [ HH.i
                        [ css "ion-trash-a" ]
                        [ ]
                    , HH.text " Delete Article"
                    ]
                ]
            _ ->
              HH.span_
                [ followButton FollowAuthor UnfollowAuthor article.author
                , HH.text " "
                , favoriteButton Medium FavoriteArticle UnfavoriteArticle article
                ]
        ]
      where
      username = article.author.username
      avatar = article.author.image
