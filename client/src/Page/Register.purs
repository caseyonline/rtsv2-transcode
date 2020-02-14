-- | The registration form allows new users to sign up to the Rtsv2App service and authenticate
-- | their session.
module Rtsv2App.Page.Register where

import Prelude

import Component.HOC.Connect as Connect
import Control.Monad.Reader (class MonadAsk)
import Data.Const (Const)
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Effect.Aff.Class (class MonadAff)
import Formless as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Rtsv2App.Api.Request (RegisterFields)
import Rtsv2App.Capability.Navigate (class Navigate, navigate)
import Rtsv2App.Capability.Resource.User (class ManageUser, registerUser)
import Rtsv2App.Component.HTML.Footer (footer)
import Rtsv2App.Component.HTML.Header as HD
import Rtsv2App.Component.HTML.MenuMain as MM
import Rtsv2App.Component.HTML.Utils (css_, safeHref)
import Rtsv2App.Data.Email (Email)
import Rtsv2App.Data.Profile (Profile)
import Rtsv2App.Data.Route (Route(..))
import Rtsv2App.Data.Username (Username)
import Rtsv2App.Env (UserEnv)
import Rtsv2App.Form.Field as Field
import Rtsv2App.Form.Validation as V

-------------------------------------------------------------------------------
-- Types for Register Page (which we might not need)
-------------------------------------------------------------------------------
type State =
  { currentUser :: Maybe Profile }

newtype RegisterForm r f = RegisterForm (r
  ( username :: f V.FormError String Username
  , email :: f V.FormError String Email
  , password :: f V.FormError String String
  ))

derive instance newtypeRegisterForm :: Newtype (RegisterForm r f) _

data Action
  = HandleRegisterForm RegisterFields
  | Receive { currentUser :: Maybe Profile }

type ChildSlots =
  ( formless :: F.Slot RegisterForm (Const Void) () RegisterFields Unit
  , mainMenu :: MM.Slot Unit
  , header :: MM.Slot Unit
  )

-------------------------------------------------------------------------------
-- Component
-------------------------------------------------------------------------------
component
  :: forall m r
   . MonadAff m
  => ManageUser m
  => MonadAsk { userEnv :: UserEnv | r } m
  => Navigate m
  => H.Component HH.HTML (Const Void) {} Void m
component = Connect.component $ H.mkComponent
  { initialState
  , render
  , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      , receive = Just <<< Receive
      }
  }
  where
  initialState { currentUser } =
    { currentUser }
    
  handleAction = case _ of
    HandleRegisterForm fields ->
      registerUser fields >>= traverse_ (\_ -> navigate Dashboard)
    Receive { currentUser } ->
      H.modify_ _ { currentUser = currentUser }

  render :: State -> H.ComponentHTML Action ChildSlots m
  render state@{ currentUser } =
    HH.div
      [ css_ "main" ]
      [ HH.slot (SProxy :: _ "header") unit HD.component { currentUser, route: Register } absurd
      , HH.slot (SProxy :: _ "mainMenu") unit MM.component { currentUser , route: Register } absurd
      , HH.div
        [ css_ "app-content content" ]
        [ HH.div
          [ css_ "content-wrapper" ]
          [ HH.div
            [ css_ "content-header row" ]
            [ HH.div
              [ css_ "content-header-left col-md-4 col-12 mb-2" ]
              [ HH.h3
                [ css_ "content-header-h3" ]
                [ HH.text "Register" ]
              ]
            ]
          , HH.div
            [ css_ "content-body" ]
            [ HH.div
              [ css_ "row" ]
              [ HH.div
                [ css_ "col-12" ]
                [ HH.div
                  [ css_ "card" ]
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
        [ HH.h1
            [ css_ "text-xs-center"]
            [ HH.text "Sign Up" ]
        , HH.p
            [ css_ "text-xs-center" ]
            [ HH.a
                [ safeHref Login ]
                [ HH.text "Already have an account?" ]
          ]
        , HH.slot F._formless unit formComponent unit (Just <<< HandleRegisterForm)
        ]

      formComponent :: F.Component RegisterForm (Const Void) () Unit RegisterFields m
      formComponent = F.component formInput $ F.defaultSpec
        { render = renderForm
        , handleEvent = F.raiseResult
        }
        where
        formInput :: Unit -> F.Input' RegisterForm m
        formInput _ =
          { validators: RegisterForm
              { username: V.required >>> V.usernameFormat
              , email: V.required >>> V.minLength 3 >>> V.emailFormat
              , password: V.required >>> V.minLength 8 >>> V.maxLength 20
              }
          , initialInputs: Nothing
          }

        renderForm { form } =
          HH.form_
            [ HH.fieldset_
              [ username
              , email
              , password
              ]
            , Field.submit "Sign up"
            ]
          where
          proxies = F.mkSProxies (F.FormProxy :: _ RegisterForm)

          username =
            Field.input proxies.username form
              [ HP.placeholder "Username", HP.type_ HP.InputText ]

          email =
            Field.input proxies.email form
              [ HP.placeholder "Email", HP.type_ HP.InputEmail ]

          password =
            Field.input proxies.password form
              [ HP.placeholder "Password" , HP.type_ HP.InputPassword ]
