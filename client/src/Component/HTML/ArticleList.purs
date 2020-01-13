-- | This module exports a pure HTML function to render lists of articles in various formats,
-- | included paginated lists.
module Rtsv2App.Component.HTML.ArticleList where

import Prelude

import Rtsv2App.Component.HTML.Utils (css, safeHref, whenElem)
import Rtsv2App.Component.Part.FavoriteButton (favoriteButton, ButtonSize(..))
import Rtsv2App.Data.Article (ArticleWithMetadata)
import Rtsv2App.Data.Avatar as Avatar
import Rtsv2App.Data.PaginatedArray (PaginatedArray)
import Rtsv2App.Data.PreciseDateTime as PDT
import Rtsv2App.Data.Route (Route(..))
import Rtsv2App.Data.Username as Username
import Data.Array (mapWithIndex)
import Data.Enum (enumFromTo)
import Data.Foldable (length)
import Data.Maybe (Maybe(..))
import Data.Monoid (guard)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Network.RemoteData (RemoteData(..))
import Web.UIEvent.MouseEvent (MouseEvent)

articleList 
  :: forall props act
   . (Int -> act)
  -> (Int -> act)
  -> RemoteData String (PaginatedArray ArticleWithMetadata) 
  -> HH.HTML props act
articleList favoriteAct unfavoriteAct = case _ of
  NotAsked -> 
    text "Articles not yet loaded"
  Loading -> 
    text "Loading..."
  Failure err -> 
    text ("Error loading articles: " <> err)
  Success { body } | length body == 0 -> 
    text "No articles are here...yet!"
  Success articles -> 
    HH.div_ 
      (articlePreview favoriteAct unfavoriteAct `mapWithIndex` articles.body)
  where
  text str = 
    HH.div
      [ css "article-preview" ]
      [ HH.text str ]

articlePreview 
  :: forall props act
   . (Int -> act)
  -> (Int -> act)
  -> Int
  -> ArticleWithMetadata 
  -> HH.HTML props act
articlePreview favoriteAct unfavoriteAct ix article =
  HH.div
    [ css "article-preview" ]
    [ HH.div
        [ css "article-meta" ]
        [ HH.a
            [ safeHref $ Profile username ]
            [ HH.img
                [ HP.src $ Avatar.toStringWithDefault avatar
                , HP.alt $ Username.toString username
                ]
            ]
        , HH.div
            [ css "info" ]
            [ HH.a
                [ css "author", safeHref $ Profile username ]
                [ HH.text $ Username.toString username ]
            , HH.span
                [ css "date" ]
                [ HH.text $ PDT.toDisplayWeekName article.createdAt ]
            ]
        , HH.div
            [ css "pull-xs-right" ]
            [ favoriteButton Icon (favoriteAct ix) (unfavoriteAct ix) article ]
        ]
    , HH.a
        [ css "preview-link" 
        , safeHref $ ViewArticle article.slug
        ]
        [ HH.h1_ 
            [ HH.text article.title ]
        , HH.p_ 
            [ HH.text article.description ]
        , HH.span_ 
            [ HH.text "Read more..." ]
        , HH.ul
            [ css "tag-list" ]
            (article.tagList <#> renderTag)
        ]
    ]
  where
  username = article.author.username
  avatar = article.author.image

renderTag :: forall props act. String -> HH.HTML props act
renderTag tag =
  HH.li
    [ css "tag-default tag-pill tag-outline" ]
    [ HH.text tag ]

-- Pagination

renderPagination 
  :: forall props act
   . (Int -> MouseEvent -> act) 
  -> Int 
  -> PaginatedArray ArticleWithMetadata 
  -> HH.HTML props act
renderPagination act currentIndex { body, total } =
  whenElem (total > 20) \_ ->
    HH.ul  
      [ css "pagination" ]
      (renderPageLink act currentIndex <$> enumFromTo 1 (total / 20))

renderPageLink 
  :: forall props act
   . (Int -> MouseEvent -> act) 
  -> Int 
  -> Int 
  -> HH.HTML props act
renderPageLink act activeIndex index =
  HH.li
    [ css $ "page-item" <> guard (activeIndex == index) " active" ]
    [ HH.a 
        [ css "page-link"
        , HP.href "" -- needed for realworld css; remember to prevent default! 
        , HE.onClick $ Just <<< act index
        ]
        [ HH.text $ show index ]
    ]