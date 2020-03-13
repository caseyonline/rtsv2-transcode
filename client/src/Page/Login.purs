-- | The login page supports a form users can submit to authenticate their session and gain access
-- | to the application.
module Rtsv2App.Page.Login where

import Prelude

import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Foreign.FrontEnd as FF
import Formless as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Rtsv2App.Api.Request (LoginFields)
import Rtsv2App.Capability.Navigate (class Navigate, navigate)
import Rtsv2App.Component.Utils (NotificationMessage)
import Rtsv2App.Capability.Resource.User (class ManageUser, loginUser)
import Rtsv2App.Component.HTML.Header as HD
import Rtsv2App.Component.HTML.Menu.MenuMain as MM
import Rtsv2App.Component.HTML.Utils (css_, safeHref, whenElem)
import Rtsv2App.Data.Email (Email)
import Rtsv2App.Data.Route (Route(..))
import Rtsv2App.Env (changeHtmlClass)
import Rtsv2App.Form.Field as Field
import Rtsv2App.Form.Validation as V

-------------------------------------------------------------------------------
-- Types for Login Page
-------------------------------------------------------------------------------
data Action
  = Initialize
  | HandleLoginForm LoginFields

type State =
  { redirect :: Boolean }

type Input =
  { redirect :: Boolean }

type ChildSlots =
  ( formless :: F.Slot LoginForm FormQuery () LoginFields Unit
  , mainMenu :: MM.Slot Unit
  , header :: HD.Slot Unit
  )

component
  :: forall m
   . MonadAff m
  => Navigate m
  => ManageUser m
  => H.Component HH.HTML (Const Void) Input NotificationMessage m
component = H.mkComponent
  { initialState: identity
  , render
  , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      , initialize = Just Initialize
      }
  }
  where
  handleAction :: Action -> H.HalogenM State Action ChildSlots NotificationMessage m Unit
  handleAction = case _ of
    Initialize -> do
      _ <- liftEffect $ changeHtmlClass ""
      -- theme initialisation
      liftEffect $ FF.init
      
    HandleLoginForm fields -> do
      -- loginUser also handles broadcasting the user changes to subscribed components
      -- so they receive the up-to-date value (see AppM and the `authenticate` function.)
      loginUser fields >>= case _ of
        Nothing ->
          void $ H.query F._formless unit $ F.injQuery $ SetLoginError true unit
        Just profile -> do
          void $ H.query F._formless unit $ F.injQuery $ SetLoginError false unit
          st <- H.get
          when st.redirect (navigate DashboardR)

  render :: State -> H.ComponentHTML Action ChildSlots m
  render _  =
     HH.section
      [ css_ "section hero is-fullheight is-error-section" ]
      [ HH.div
        [ css_ "hero-body" ]
        [ HH.div
          [ css_ "container" ]
          [ HH.div
            [ css_ "columns is-centered" ]
            [ HH.div
              [ css_ "column is-two-fifths" ]
              [ HH.div
                [css_ "card has-card-header-background" ]
                [ HH.header
                  [ css_ "card-header" ]
                  [ HH.p
                    [ css_ "card-header-title" ]
                    [ HH.span
                      [ css_ "icon"]
                      [ HH.i
                        [ css_ "mdi mdi-lock default" ]
                        []
                      ]
                    , HH.span_
                      [ HH.text "Login" ]
                    ]
                  , HH.a
                    [ css_ "button is-small"
                    , safeHref RegisterR ]
                    [ HH.text "Need an account?" ]
                  ]
                , HH.div
                  [ css_ "card-content" ]
                  [ HH.slot F._formless unit formComponent unit (Just <<< HandleLoginForm) ]
                ]
              ]
            ]
          ]
        ]
      ]


newtype LoginForm r f = LoginForm (r
  ( email :: f V.FormError String Email
  , password :: f V.FormError String String
  ))

derive instance newtypeLoginForm :: Newtype (LoginForm r f) _

-- Extend the form to receive more queries than it supports by default. Here, we'll
-- set a login error from the parent.
data FormQuery a
  = SetLoginError Boolean a

derive instance functorFormQuery :: Functor (FormQuery)

formComponent :: forall m. MonadAff m => F.Component LoginForm FormQuery () Unit LoginFields m
formComponent = F.component formInput $ F.defaultSpec
  { render = renderLogin
  , handleEvent = handleEvent
  , handleQuery = handleQuery
  }
  where
  formInput :: Unit -> F.Input LoginForm (loginError :: Boolean) m
  formInput _ =
    { validators: LoginForm
        { email: V.required >>> V.minLength 3 >>> V.emailFormat
        , password: V.required >>> V.minLength 2 >>> V.maxLength 20
        }
    , initialInputs: Nothing
    , loginError: false
    }

  handleEvent = F.raiseResult

  handleQuery :: forall a. FormQuery a -> H.HalogenM _ _ _ _ _ (Maybe a)
  handleQuery = case _ of
    SetLoginError bool a -> do
      H.modify_ _ { loginError = bool }
      pure (Just a)

  proxies = F.mkSProxies (F.FormProxy :: _ LoginForm)

  renderLogin { form, loginError } =
    HH.form_
      [ whenElem loginError \_ ->
          HH.div
            [ css_ "error-message has-text-danger" ]
            [ HH.text "Email or password is invalid" ]
      , HH.div
        [ css_ "field"]
        [ HH.label
          [ css_ "label" ]
          [ HH.text "E-mail Address"]
        , HH.div
          [ css_ "control"]
          [ Field.input proxies.email form
            [ HP.placeholder "Email"
            , HP.type_ HP.InputEmail
            ]
          ]
        ]
      , HH.div
        [ css_ "field"]
        [ HH.label
          [ css_ "label" ]
          [ HH.text "Password"]
        , HH.div
          [ css_ "control"]
          [ Field.input proxies.password form
            [ HP.placeholder "Password"
            , HP.type_ HP.InputPassword
            ]
          ]
        ]
      , HH.hr_
      , HH.div
        [ css_ "field is-grouped"]
        [ HH.div
          [ css_ "control"]
          [ Field.submit "Login" ]
        ]
      ]
