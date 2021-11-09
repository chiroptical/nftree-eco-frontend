module Form.RegisterLogin where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.User (User)
import Effect.Aff.Class (class MonadAff)
import Formless as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Tailwind as T
import Type.Proxy (Proxy(..))
import Web.Event.Event as Event
import Css.Input as Css

-- TODO: Need to add validation for these fields
newtype RegistrationForm (r :: Row Type -> Type) f
  = RegistrationForm
  ( r
      -- f {error} {input} {output}
      ( email :: f Void String String
      , password :: f Void String String
      )
  )

derive instance newtypeRegistrationForm :: Newtype (RegistrationForm r f) _

data FormAction
  = Submit Event.Event

type Props
  =
  { buttonText :: String
  }

formComponent
  :: forall formQuery formSlots formInput m
   . MonadAff m
  => Props
  -> F.Component RegistrationForm formQuery formSlots formInput User m
formComponent { buttonText } =
  F.component formInput
    $ F.defaultSpec
        { render = renderForm
        , handleEvent = handleEvent
        , handleAction = handleAction
        }
  where
  formInput _ =
    { initialInputs: Nothing -- same as: Just (F.wrapInputFields { name: "", age: "" })
    , validators:
        RegistrationForm
          { email: F.noValidation
          , password: F.noValidation
          }
    }

  handleEvent = F.raiseResult

  handleAction = case _ of
    Submit event -> do
      H.liftEffect $ Event.preventDefault event
      F.handleAction handleAction handleEvent F.submit

  _email = Proxy :: Proxy "email"

  _password = Proxy :: Proxy "password"

  renderForm { form } =
    HH.form
      [ HP.classes [ T.mt8, T.spaceY6 ]
      , HE.onSubmit $ F.injAction <<< Submit
      ]
      [ HH.fieldset_
          [ HH.input
              [ HP.value $ F.getInput _email form
              , HE.onValueInput $ F.setValidate _email
              , HP.type_ HP.InputEmail
              , HP.placeholder "Email address"
              , HP.name "email"
              , HP.id "email"
              , HP.classes Css.input
              ]
          , HH.input
              [ HP.value $ F.getInput _password form
              , HE.onValueInput $ F.set _password
              , HP.type_ HP.InputPassword
              , HP.placeholder "Password"
              , HP.name "password"
              , HP.id "password"
              , HP.classes Css.input
              ]
          ]
      , HH.input
          [ HP.type_ HP.InputSubmit
          , HP.value buttonText
          , HP.classes Css.inputSubmit
          ]
      ]
