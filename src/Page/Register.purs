module Page.Register where

import Prelude
import Data.Argonaut.Core as A
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Aff as Aff
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Effect.Class.Console (logShow)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Milkis as M
import Milkis.Impl.Window (windowFetch)
import Service.Navigate (class Navigate)
import Tailwind as T
import Type.Proxy (Proxy(..))
import Formless as F
import Data.User (User)
import Form.Registration (registrationFormInput, registrationFormSpec)

data Action
  = HandleRegistrationForm User

type Slot p
  = forall query. H.Slot query Void p

type State
  = { statusCode :: Maybe Int }

_register :: Proxy "register"
_register = Proxy

fetch :: M.Fetch
fetch = M.fetch windowFetch

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
    , eval:
        H.mkEval
          $ H.defaultEval
              { handleAction = handleAction
              , initialize = Nothing
              }
    }
  where
  handleAction = case _ of
    HandleRegistrationForm user -> logShow user

  render _ =
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
          [ HH.div
              []
              [ HH.img
                  [ HP.alt "Workflow"
                  , HP.src "https://tailwindui.com/img/logos/workflow-mark-indigo-600.svg"
                  , HP.classes [ T.mxAuto, T.h12, T.wAuto ]
                  ]
              , HH.h2
                  [ HP.classes [ T.mt6, T.textCenter, T.text3xl, T.fontExtrabold, T.textGray900 ] ]
                  [ HH.text "Register for an account" ]
              ]
          ]
      , HH.slot
          F._formless
          unit
          (F.component (const registrationFormInput) registrationFormSpec)
          unit
          HandleRegistrationForm
      ]

--  Initialize -> do
--    response_ <-
--      H.liftAff
--        $ Aff.attempt
--        $ fetch
--            -- TODO: Abstract backend urls to some record type
--            -- TODO: Abstract post requests
--            (M.URL $ "http://localhost:8081/auth/login")
--            { method: M.postMethod
--            , credentials: M.includeCredentials
--            , body:
--                A.stringify
--                  $ CA.encode userCodec
--                      { username: "chiroptical", password: "chiroptical" }
--            , headers:
--                M.makeHeaders
--                  { "Content-Type": "application/json"
--                  }
--            }
--    case response_ of
--      Right response -> H.put { statusCode: Just $ M.statusCode response }
--      Left e -> log $ "Left: " <> show e
