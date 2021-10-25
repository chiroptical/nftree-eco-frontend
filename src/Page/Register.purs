module Page.Register where

import Prelude
import Data.User (User)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Effect.Class.Console (logShow)
import Form.Registration (formComponent)
import Formless as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Service.Navigate (class Navigate)
import Tailwind as T
import Type.Proxy (Proxy(..))

data Action
  = HandleRegistrationForm User

type Slot p
  = forall query. H.Slot query Void p

_register :: Proxy "register"
_register = Proxy

component ::
  forall q s o m.
  MonadAff m =>
  MonadEffect m =>
  Navigate m =>
  H.Component q s o m
component =
  H.mkComponent
    { initialState: identity
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
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
          formComponent
          unit
          HandleRegistrationForm
      ]

-- fetch :: M.Fetch
-- fetch = M.fetch windowFetch
-- import Milkis as M
-- import Milkis.Impl.Window (windowFetch)
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
