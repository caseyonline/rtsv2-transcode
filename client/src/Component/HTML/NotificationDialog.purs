module Rtsv2App.Component.HTML.NotificationDialog where


import Prelude

import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Effect.Aff (delay)
import Effect.Aff.Class (class MonadAff, liftAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Rtsv2App.Component.Utils (Notification(..), NotificationContent, unwrapNotification)
import Rtsv2App.Component.HTML.Utils (css_)


data Action
  = CloseDialog Int
  | Receive Input
  | Initialize

data Message = NDialogMsg Int

type State = NDialogContent

type NDialogContent =
  { message   :: String
  , cssStyle  :: String
  , index     :: Int
  , title     :: String
  , fadeStyle :: FadeStyle
  , autoClose :: Boolean
  }

data FadeStyle
  = FadeInUp
  | FadeOut

type Input =
  { notification :: Notification NotificationContent
  , index :: Int
  }

type Slot = H.Slot (Const Void) Message


component
  :: forall m
   . MonadAff m
  => H.Component HH.HTML (Const Void) Input Message m
component = H.mkComponent
  { initialState: initialState
  , render
  , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      , receive = Just <<< Receive
      , initialize = Just Initialize
      }
  }
  where
  initialState :: Input -> State
  initialState notification = do
    toNDialogContent notification

  handleAction :: Action -> H.HalogenM State Action () Message m Unit
  handleAction = case _ of
    Initialize -> do
      st <- H.get
      case st.autoClose of
        true  -> do
          _ <- liftAff $ delay $ Milliseconds 2000.0
          handleAction $ CloseDialog st.index

        false -> pure unit

    Receive notification -> do
      H.put $ toNDialogContent notification
      handleAction $ Initialize

    CloseDialog index -> H.raise $ NDialogMsg index

  render state@{ message, cssStyle, index, title, fadeStyle, autoClose } =
    HH.article
    [ css_ $ "snackbar message is-bottom-right is-small" <> " " <> cssStyle <> " " <> (whichFadeStyle fadeStyle) ]
    [ HH.div
      [ css_ "message-header" ]
      [ HH.text title
      , HH.button
        [ css_ "delete"
        , HE.onClick $ const $ Just $ CloseDialog index
        ]
        []
      ]
    , HH.div
      [ css_ $ "message-body" ]
      [ HH.p_
        [ HH.text message ]
      ]
    ]

toNDialogContent :: Input -> State
toNDialogContent { notification, index } = do
  let ntf = unwrapNotification notification
  { message: ntf.message
  , cssStyle: inputTocssStyle notification
  , index
  , title: ntf.title
  , fadeStyle: FadeInUp
  , autoClose: ntf.autoClose
  }

inputTocssStyle :: Notification NotificationContent -> String
inputTocssStyle notification =
  case notification of
    DangerN  n -> "is-danger"
    DarkN    n -> "is-dark"
    InfoN    n -> "is-info"
    LightN   n -> "is-light"
    PrimaryN n -> "is-primary"
    SuccessN n -> "is-success"
    WarningN n -> "is-warning"

whichFadeStyle :: FadeStyle -> String
whichFadeStyle = case _ of
  FadeInUp -> "fadeInUp"
  FadeOut  -> "fadeOut"



-- <div class="notices is-bottom">
--   <div role="alertdialog" class="snackbar is-success is-bottom-right fadeOut v-leave-to" style="">
--     <div class="text">Got click</div>
--     <div class="action is-success">
--       <button class="button">OK</button>
--     </div>
--   </div>
-- </div>
