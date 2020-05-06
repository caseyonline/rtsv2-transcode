module Rtsv2App.Page.Settings where

import Prelude

import Control.Monad.Reader (class MonadAsk, ask)
import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Data.Newtype (class Newtype, unwrap)
import Data.Symbol (SProxy(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Ref as Ref
import Formless as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Network.RemoteData (RemoteData(..), fromMaybe)
import Rtsv2App.Capability.Navigate (class Navigate, logout)
import Rtsv2App.Capability.Resource.User (class ManageUser, UpdateProfileFields, getCurrentUser, updateUser)
import Rtsv2App.Component.HTML.Footer (footer)
import Rtsv2App.Component.HTML.Header as HD
import Rtsv2App.Component.HTML.Menu.MenuWrapper as MW
import Rtsv2App.Component.HTML.Utils (css_)
import Rtsv2App.Component.Utils (NotificationMessage)
import Rtsv2App.Data.Avatar (Avatar)
import Rtsv2App.Data.Avatar as Avatar
import Rtsv2App.Data.Email (Email)
import Rtsv2App.Data.Profile (ProfileEmail, Profile)
import Rtsv2App.Data.Route (Route(..))
import Rtsv2App.Data.Username (Username)
import Rtsv2App.Data.Username as Username
import Rtsv2App.Env (UserEnv, PoPDefEnv)
import Rtsv2App.Form.Field as Field
import Rtsv2App.Form.Validation as V

-------------------------------------------------------------------------------
-- Types for Settings Page
-------------------------------------------------------------------------------
newtype SettingsForm r f = SettingsForm (r
  ( image :: f V.FormError String (Maybe Avatar)
  , username :: f V.FormError String Username
  , bio :: f Void String (Maybe String)
  , email :: f V.FormError String Email
  , password :: f V.FormError String (Maybe String)
  ))

derive instance newtypeSettingsForm :: Newtype (SettingsForm r f) _

type Input =
  { currentUser :: Maybe Profile }

data Action
  = Initialize
  | HandleForm UpdateProfileFields
  | LogUserOut
  | Receive Input

type State =
  { profile :: RemoteData String ProfileEmail
  , currentUser :: Maybe Profile
  }

type ChildSlots =
  ( menuWrapper :: MW.Slot Unit
  , header      :: HD.Slot Unit
  , formless    :: F.Slot SettingsForm (Const Void) () UpdateProfileFields Unit
  )

-------------------------------------------------------------------------------
-- Component
-------------------------------------------------------------------------------
component
  :: forall m r
   . MonadAff m
  => Navigate m
  => MonadAsk { userEnv :: UserEnv, popDefEnv :: PoPDefEnv | r } m
  => ManageUser m
  => H.Component HH.HTML (Const Void) Input NotificationMessage m
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
  initialState { currentUser } =
    { currentUser
    , profile: NotAsked
    }

  handleAction :: Action -> H.HalogenM State Action ChildSlots NotificationMessage m Unit
  handleAction = case _ of
    Initialize -> do
      { userEnv } <- ask
      mbProfile <- H.liftEffect $ Ref.read userEnv.currentUser
      st <- H.modify _ { currentUser = mbProfile }
      H.modify_ _ { profile = Loading }
      mbProfileWithEmail <- getCurrentUser
      H.modify_ _ { profile = fromMaybe mbProfileWithEmail }

      -- if profile cann't be located then something horrible has gone wrong
      -- then log the user out
      case mbProfileWithEmail of
        Nothing -> logout
        Just profile -> do
          let
            newInputs = F.wrapInputFields
              { image: Maybe.fromMaybe "" $ Avatar.toString <$> profile.image
              , username: Username.toString profile.username
              , bio: Maybe.fromMaybe "" profile.bio
              , email: unwrap profile.email
              , password: ""
              }
          void $ H.query F._formless unit $ F.asQuery $ F.loadForm newInputs

    HandleForm fields -> do
      updateUser fields
      mbProfileWithEmail <- getCurrentUser
      H.modify_ _ { profile = fromMaybe mbProfileWithEmail }

    LogUserOut -> logout

    Receive { currentUser } -> do pure unit

  render :: State -> H.ComponentHTML Action ChildSlots m
  render state@{ profile, currentUser } =
    HH.div
      [ css_ "main" ]
      [ HH.slot (SProxy :: _ "header") unit HD.component { currentUser, route: SettingsR } absurd
      , HH.slot (SProxy :: _ "menuWrapper") unit MW.component { curPopName: Nothing, currentUser, route: DashboardR } absurd
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
                [ HH.text "Settings" ]
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
        [ HH.div
            [ css_ "card-header" ]
            [ HH.text "Your Settings" ]
        , HH.div
            [ css_ "card-content collapse show" ]
            [ HH.div
              [ css_ "card-body" ]
              [ HH.slot F._formless unit formComponent unit (Just <<< HandleForm)
              , HH.hr_
              , HH.button
                [ css_ "btn btn-outline-danger"
                , HE.onClick \_ -> Just LogUserOut
                ]
                [ HH.text "Log out" ]
              ]
            ]
        ]


      formComponent :: F.Component SettingsForm (Const Void) () Unit UpdateProfileFields m
      formComponent = F.component formInput $ F.defaultSpec
        { render = renderForm
        , handleEvent = F.raiseResult
        }
        where
        formInput :: Unit -> F.Input' SettingsForm m
        formInput _ =
          { validators: SettingsForm
              { image: V.toOptional V.avatarFormat
              , username: V.required >>> V.minLength 3 >>> V.maxLength 20 >>> V.usernameFormat
              , bio: F.hoistFn_ pure
              , email: V.required >>> V.minLength 3 >>> V.maxLength 50 >>> V.emailFormat
              , password: V.toOptional $ V.minLength 3 >>> V.maxLength 20
              }
          , initialInputs: Nothing
          }

        renderForm { form } =
          HH.form_
            [ HH.fieldset_
              [ image
              , username
              , bio
              , email
              , password
              , Field.submit "Update settings"
              ]
            ]
          where
          proxies = F.mkSProxies (F.FormProxy :: _ SettingsForm)

          image =
            Field.input proxies.image form
              [ HP.placeholder "URL of profile picture", HP.type_ HP.InputText ]

          username =
            Field.input proxies.username form
              [ HP.placeholder "Your name", HP.type_ HP.InputText ]

          bio =
            HH.fieldset
              [ css_ "form-group" ]
              [ HH.textarea
                  [ css_ "form-control form-control-lg"
                  , HP.placeholder "Short bio about you"
                  , HP.rows 8
                  , HP.value $ F.getInput proxies.bio form
                  , HE.onValueInput $ Just <<< F.setValidate proxies.bio
                  ]
              ]

          email =
            Field.input proxies.email form
              [ HP.placeholder "Email", HP.type_ HP.InputEmail ]

          password =
            Field.input proxies.password form
              [ HP.placeholder "Password", HP.type_ HP.InputPassword ]
