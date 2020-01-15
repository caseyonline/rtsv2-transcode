-- | The login page supports a form users can submit to authenticate their session and gain access
-- | to the application.
module Rtsv2App.Page.Login where

import Prelude

import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Effect.Aff.Class (class MonadAff)
import Formless as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Rtsv2App.Api.Request (LoginFields)
import Rtsv2App.Capability.Navigate (class Navigate, navigate)
import Rtsv2App.Capability.Resource.User (class ManageUser, loginUser)
import Rtsv2App.Component.HTML.Footer (footer)
import Rtsv2App.Component.HTML.MainMenu as MM
import Rtsv2App.Component.HTML.Header as HD
import Rtsv2App.Component.HTML.Utils (css, safeHref, whenElem)
import Rtsv2App.Data.Email (Email)
import Rtsv2App.Data.Route (Route(..))
import Rtsv2App.Form.Field (submit)
import Rtsv2App.Form.Field as Field
import Rtsv2App.Form.Validation as V

data Action
  = HandleLoginForm LoginFields

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
  => H.Component HH.HTML (Const Void) Input Void m
component = H.mkComponent
  { initialState: identity
  , render
  , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
  }
  where
  handleAction :: Action -> H.HalogenM State Action ChildSlots Void m Unit
  handleAction = case _ of
    HandleLoginForm fields -> do
      -- loginUser also handles broadcasting the user changes to subscribed components
      -- so they receive the up-to-date value (see AppM and the `authenticate` function.)
      loginUser fields >>= case _ of
        Nothing ->
          void $ H.query F._formless unit $ F.injQuery $ SetLoginError true unit
        Just profile -> do
          void $ H.query F._formless unit $ F.injQuery $ SetLoginError false unit
          st <- H.get
          when st.redirect (navigate Home)

  render :: State -> H.ComponentHTML Action ChildSlots m
  render _  =
     HH.div
      [ css "main" ]
      [ HH.slot (SProxy :: _ "header") unit HD.component { currentUser: Nothing , route: Login } absurd
      , HH.slot (SProxy :: _ "mainMenu") unit MM.component { currentUser: Nothing , route: Login } absurd
      , HH.div
        [ css "app-content content" ]
        [ HH.div
          [ css "content-wrapper" ]
          [ HH.div
            [ css "content-wrapper-before" ]
            []
          , HH.div
            [ css "content-header row" ]
            [ HH.div
              [ css "content-header-left col-md-4 col-12 mb-2" ]
              [ HH.h3
                [ css "content-header-h3" ]
                [ HH.text "Login" ]
              ]
            ]
          , HH.div
            [ css "content-body" ]
            [ HH.div
              [ css "row" ]
              [ HH.div
                [ css "col-12" ]
                [ HH.div
                  [ css "card" ]
                  html
                ]
              ]
            ]
          ]
        ]
      , footer
      ]
    where
      html =
        [ HH.div
            [ css "card-header" ]
            [ HH.text "Sign In" ]
        , HH.div
            [ css "card-content collapse show" ]
            [ HH.div
              [ css "card-body" ]
              [ HH.a
                [ safeHref Register ]
                [ HH.text "Need an account?" ]
              , HH.slot F._formless unit formComponent unit (Just <<< HandleLoginForm)
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
            [ css "error-messages" ]
            [ HH.text "Email or password is invalid" ]
      , HH.fieldset_
          [ Field.input proxies.email form
              [ HP.placeholder "Email"
              , HP.type_ HP.InputEmail
              ]
          , Field.input proxies.password form
              [ HP.placeholder "Password"
              , HP.type_ HP.InputPassword
              ]
          , submit "Log in"
          ]
      ]
