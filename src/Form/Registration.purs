module Form.Registration where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Formless as F
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Tailwind as T
import Type.Proxy (Proxy(..))
import Web.Event.Event as Event
import Halogen as H
import Effect.Aff.Class (class MonadAff)

newtype RegistrationForm (r :: Row Type -> Type) f
  = RegistrationForm
  ( r
      -- f {error} {input} {output}
      ( username :: f Void String String
      , email :: f Void String String
      , password :: f Void String String
      )
  )

derive instance newtypeRegistrationForm :: Newtype (RegistrationForm r f) _

type RegistrationFields
  = { email :: String
    , password :: String
    , username :: String
    }

data FormAction
  = Submit Event.Event

formComponent ::
  forall formQuery formSlots formInput m.
  MonadAff m =>
  F.Component RegistrationForm formQuery formSlots formInput RegistrationFields m
formComponent =
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
          { username: F.noValidation
          , email: F.noValidation
          , password: F.noValidation
          }
    }

  handleEvent = F.raiseResult

  handleAction = case _ of
    Submit event -> do
      H.liftEffect $ Event.preventDefault event
      F.handleAction handleAction handleEvent F.submit

  _username = Proxy :: Proxy "username"

  _email = Proxy :: Proxy "email"

  _password = Proxy :: Proxy "password"

  renderForm { form } =
    HH.form
      [ HP.classes [ T.mt8, T.spaceY6 ]
      , HE.onSubmit $ F.injAction <<< Submit
      ]
      [ HH.fieldset_
          [ HH.input
              [ HP.placeholder "Username"
              , HP.classes
                  [ T.appearanceNone
                  , T.roundedNone
                  , T.relative
                  , T.block
                  , T.wFull
                  , T.px3
                  , T.py2
                  , T.border
                  , T.borderGray300
                  , T.placeholderGray500
                  , T.textGray900
                  , T.roundedTMd
                  , T.focusOutlineNone
                  , T.focusRingIndigo500
                  , T.focusBorderIndigo500
                  , T.focusZ10
                  , T.smTextSm
                  ]
              , HP.value $ F.getInput _username form
              , HE.onValueInput $ F.setValidate _username
              , HP.type_ HP.InputText
              , HP.name "username"
              , HP.id "username"
              ]
          , HH.input
              [ HP.placeholder "Email address"
              , HP.classes
                  [ T.appearanceNone
                  , T.roundedNone
                  , T.relative
                  , T.block
                  , T.wFull
                  , T.px3
                  , T.py2
                  , T.border
                  , T.borderGray300
                  , T.placeholderGray500
                  , T.textGray900
                  , T.roundedBMd
                  , T.focusOutlineNone
                  , T.focusRingIndigo500
                  , T.focusBorderIndigo500
                  , T.focusZ10
                  , T.smTextSm
                  ]
              , HP.value $ F.getInput _email form
              , HE.onValueInput $ F.setValidate _email
              , HP.type_ HP.InputEmail
              , HP.name "email"
              , HP.id "email"
              ]
          , HH.input
              [ HP.placeholder "Password"
              , HP.classes
                  [ T.appearanceNone
                  , T.roundedNone
                  , T.relative
                  , T.block
                  , T.wFull
                  , T.px3
                  , T.py2
                  , T.border
                  , T.borderGray300
                  , T.placeholderGray500
                  , T.textGray900
                  , T.roundedBMd
                  , T.focusOutlineNone
                  , T.focusRingIndigo500
                  , T.focusBorderIndigo500
                  , T.focusZ10
                  , T.smTextSm
                  ]
              , HP.value $ F.getInput _password form
              , HE.onValueInput $ F.set _password
              , HP.type_ HP.InputPassword
              , HP.name "password"
              , HP.id "password"
              ]
          ]
      , HH.input
          [ HP.type_ HP.InputSubmit
          , HP.value "Register"
          , HP.classes
              [ T.group
              , T.relative
              , T.wFull
              , T.flex
              , T.justifyCenter
              , T.py2
              , T.px4
              , T.border
              , T.borderTransparent
              , T.textSm
              , T.fontMedium
              , T.roundedMd
              , T.textWhite
              , T.bgIndigo600
              , T.hoverBgIndigo700
              , T.focusOutlineNone
              , T.focusRing2
              , T.focusRingOffset2
              , T.focusRingIndigo500
              ]
          ]
      ]
