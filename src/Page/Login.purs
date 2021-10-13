module Page.Login where

import Prelude
import Effect.Class (class MonadEffect)
import Type.Proxy (Proxy(..))
import Halogen as H
import Effect.Class.Console (log)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Halogen.HTML as HH
import Service.Navigate (class Navigate)
import Milkis as M
import Effect.Aff.Class (class MonadAff)
import Milkis.Impl.Window (windowFetch)
import Effect.Aff as Aff

data Action
  = Initialize

type Slot p
  = forall query. H.Slot query Void p

type State
  = { statusCode :: Maybe Int
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
    { render
    , initialState: identity
    , eval:
        H.mkEval
          $ H.defaultEval
              { handleAction = handleAction
              , initialize = Just Initialize
              }
    }

fetch :: M.Fetch
fetch = M.fetch windowFetch

handleAction ::
  forall s o m.
  MonadAff m =>
  Navigate m =>
  Action ->
  H.HalogenM State Action s o m Unit
handleAction = case _ of
  Initialize -> do
    response_ <-
      H.liftAff
        $ Aff.attempt
        $ fetch
            -- TODO: Should have some kind of abstraction here
            (M.URL $ "http://localhost:8080/login")
            M.defaultFetchOptions
    case response_ of
      Right response -> H.put { statusCode: Just $ M.statusCode response }
      Left e -> log $ "Left: " <> show e

render :: forall action m. State -> H.ComponentHTML action () m
render _ = HH.h1_ [ HH.text "Nothing here yet..." ]
