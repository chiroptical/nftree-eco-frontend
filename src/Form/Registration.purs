module Form.Registration where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Formless as F
import Data.User (User)
import Type.Proxy (Proxy(..))
import Halogen.HTML as HH
import Tailwind as T
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

newtype RegistrationForm (r :: Row Type -> Type) f
  = RegistrationForm
  ( r
      --          error    input  output
      ( username :: f Void String String
      , email :: f Void String String
      , password :: f Void String String
      )
  )

derive instance newtypeRegistrationForm :: Newtype (RegistrationForm r f) _

-- TODO: Turn this into a component, see https://github.com/thomashoneyman/purescript-halogen-formless/blob/main/example/basic/Component.purs#L32
registrationFormInput :: forall m. Monad m => F.Input' RegistrationForm m
registrationFormInput =
  { initialInputs: Nothing -- same as: Just (F.wrapInputFields { name: "", age: "" })
  , validators:
      RegistrationForm
        { username: F.noValidation
        , email: F.noValidation
        , password: F.noValidation
        }
  }

registrationFormSpec :: forall input m. Monad m => F.Spec' RegistrationForm User input m
registrationFormSpec = F.defaultSpec { render = _render, handleEvent = F.raiseResult }
  where
  _username = Proxy :: Proxy "username"

  _email = Proxy :: Proxy "email"

  _password = Proxy :: Proxy "password"

  _render { form } =
    HH.form
      [ HP.classes [ T.mt8, T.spaceY6 ]
      ]
      [ HH.div
          [ HP.classes [ T.roundedMd, T.shadowSm, T.negSpaceYPx ] ]
          [ HH.div
              []
              [ HH.label
                  [ HP.classes [ T.srOnly ], HP.for "username" ]
                  [ HH.text "Username" ]
              , HH.input
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
                  , HP.required true
                  , HP.type_ HP.InputText
                  , HP.value $ F.getInput _username form
                  , HE.onValueInput $ F.setValidate _username
                  , HP.name "username"
                  , HP.id "username"
                  ]
              ]
          , HH.div
              []
              [ HH.label
                  [ HP.classes [ T.srOnly ], HP.for "email" ]
                  [ HH.text "Email address" ]
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
                  , HP.required true
                  , HP.autocomplete true
                  , HP.type_ HP.InputEmail
                  , HP.value $ F.getInput _email form
                  , HE.onValueInput $ F.setValidate _email
                  , HP.name "email"
                  , HP.id "email"
                  ]
              ]
          , HH.div
              []
              [ HH.label
                  [ HP.classes [ T.srOnly ], HP.for "password" ]
                  [ HH.text "Password" ]
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
                  , HP.required true
                  , HP.autocomplete true
                  , HP.type_ HP.InputPassword
                  , HP.value $ F.getInput _password form
                  , HE.onValueInput $ F.set _password
                  , HP.name "password"
                  , HP.id "password"
                  ]
              ]
          ]
      , HH.div
          []
          [ HH.button
              [ HP.classes
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
              -- , HP.type_ HP.ButtonSubmit
              , HE.onClick \_ -> F.submit
              ]
              [ HH.text "Register"
              ]
          ]
      ]
