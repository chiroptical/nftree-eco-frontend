module Page.Login where

import Prelude
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.User (User, userCodec)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Effect.Class.Console (log)
import Form.RegisterLogin (formComponent)
import Formless as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Milkis (statusCode, text)
import Request as Request
import Service.Navigate (class Navigate, navigate)
import Service.Route as Route
import Tailwind as T
import Type.Proxy (Proxy(..))

data Action
  = HandleLoginForm User

type Slot p
  = forall query. H.Slot query Void p

type State
  = { loginError :: Maybe String
    }

_login :: Proxy "login"
_login = Proxy

component ::
  forall q o m.
  MonadAff m =>
  MonadEffect m =>
  Navigate m =>
  H.Component q State o m
component =
  H.mkComponent
    { initialState: identity
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }
  where
  handleAction = case _ of
    HandleLoginForm user -> do
      response_ <- H.liftAff $ Request.post Request.AuthLogin userCodec user
      -- TODO: Send user back to the home page
      -- TODO: Move error handling into form component
      case response_ of
        Right response -> do
          case statusCode response of
            204 -> navigate Route.Home
            403 -> do
              body <- H.liftAff $ text response
              H.modify_ $ _ { loginError = Just body }
            500 -> H.modify_ $ _ { loginError = Just "Something went wrong..." }
            _ -> H.modify_ $ _ { loginError = Just "TODO: email contact information" }
        Left e -> do
          log $ "Unable to handle request, got error: " <> show e
          H.modify_ $ _ { loginError = Just "Unable to handle your request..." }

  render { loginError } =
    HH.div
      [ HP.classes
          [ T.minHScreen
          , T.flex
          , T.itemsCenter
          , T.justifyCenter
          , T.bgGray50
          , T.py12
          , T.px4
          , T.smPx6
          , T.lgPx8
          ]
      ]
      [ HH.div
          [ HP.classes [ T.maxWMd, T.wFull, T.spaceY8 ] ]
          [ HH.div_
              -- TODO: We reload this svg over and over again...
              [ HH.img
                  [ HP.alt "Workflow"
                  , HP.src "https://tailwindui.com/img/logos/workflow-mark-indigo-600.svg"
                  , HP.classes [ T.mxAuto, T.h12, T.wAuto ]
                  ]
              , HH.h2
                  [ HP.classes [ T.mt6, T.textCenter, T.text3xl, T.fontExtrabold, T.textGray900 ] ]
                  [ HH.text "Login to your account" ]
              ]
          ]
      , HH.slot
          F._formless
          unit
          (formComponent { buttonText: "Login" })
          unit
          HandleLoginForm
      , -- TODO: Tailwind the heck out this
        case loginError of
          Nothing -> HH.text ""
          Just message -> HH.text message
      ]
