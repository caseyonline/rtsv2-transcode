-- | The login page supports a form users can submit to authenticate their session and gain access
-- | to the application.
module Rtsv2App.Page.Login where

import Prelude

import Component.HOC.Connect as Connect
import Control.Monad.Reader (class MonadAsk)
import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Effect.Aff.Class (class MonadAff)
import Formless as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Rtsv2App.Api.Request (LoginFields)
import Rtsv2App.Capability.Navigate (class Navigate, navigate)
import Rtsv2App.Capability.Resource.User (class ManageUser, loginUser)
import Rtsv2App.Component.HTML.Header (header)
import Rtsv2App.Component.HTML.MainMenu (mainMenu)
import Rtsv2App.Component.HTML.Utils (css, safeHref, whenElem)
import Rtsv2App.Data.Email (Email)
import Rtsv2App.Data.Profile (Profile)
import Rtsv2App.Data.Route (Route(..))
import Rtsv2App.Env (UserEnv)
import Rtsv2App.Form.Field (submit)
import Rtsv2App.Form.Field as Field
import Rtsv2App.Form.Validation as V
import Rtsv2App.Page.PageContainer (pageContainer)

data Action
  = HandleLoginForm LoginFields

-- Should this component redirect to home after login or not? If the login page is loaded
-- at the login route, then yes; if not, then it is guarding another route and should not.
type State =
  { redirect :: Boolean
  , currentUser :: Maybe Profile
  }

type Input =
  { redirect :: Boolean }

type ChildSlots =
  ( formless :: F.Slot LoginForm FormQuery () LoginFields Unit )

component
  :: forall r m
   . MonadAff m
  => MonadAsk { userEnv :: UserEnv | r } m
  => Navigate m
  => ManageUser m
  => H.Component HH.HTML (Const Void) Input Void m
component = Connect.component $  H.mkComponent
  { initialState
  , render
  , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
  }
  where
  initialState { currentUser } =
    { redirect: false
    , currentUser
    }

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
  render state@{ redirect, currentUser }  =
    pageContainer
      currentUser
      Login
      [ HH.div
          [ css "card-header"]
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

-- | See the Formless tutorial to learn how to build your own forms:
-- | https://github.com/thomashoneyman/purescript-halogen-formless

newtype LoginForm r f = LoginForm (r
  ( email :: f V.FormError String Email
  , password :: f V.FormError String String
  ))

derive instance newtypeLoginForm :: Newtype (LoginForm r f) _

-- We can extend our form to receive more queries than it supports by default. Here, we'll
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
