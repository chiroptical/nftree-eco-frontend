module Page.Register where

import Prelude
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Aff as Aff
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Milkis as M
import Milkis.Impl.Window (windowFetch)
import Service.Navigate (class Navigate)
import Type.Proxy (Proxy(..))

data Action
  = Initialize

type Slot p
  = forall query. H.Slot query Void p

type State
  = { statusCode :: Maybe Int }

_register :: Proxy "register"
_register = Proxy

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
            (M.URL $ "http://localhost:8080/register")
            M.defaultFetchOptions
    case response_ of
      Right response -> H.put { statusCode: Just $ M.statusCode response }
      Left e -> log $ "Left: " <> show e

render :: forall m. Navigate m => State -> H.ComponentHTML Action () m
render _ = HH.h1_ [ HH.text "Nothing here yet..." ]
