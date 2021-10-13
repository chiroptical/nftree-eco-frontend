module Page.InitializeRegister where

import Prelude (Void, identity, ($), bind, Unit)
import Type.Proxy (Proxy(..))
import Web.HTML (window)
import Web.HTML.Window (location)
import Web.HTML.Location (setHref)
import Effect.Class (liftEffect, class MonadEffect)
import Data.Maybe (Maybe(..))
-- Halogen
import Halogen as H
import Halogen.HTML as HH

type Slot p
  = forall query. H.Slot query Void p

data Action
  = Initialize

_initializeRegister :: Proxy "initializeRegister"
_initializeRegister = Proxy

component :: forall q o m. MonadEffect m => H.Component q Unit o m
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

handleAction :: forall s o m. MonadEffect m => Action -> H.HalogenM Unit Action s o m Unit
handleAction = case _ of
  Initialize ->
    liftEffect
      $ do
          _window <- window
          _location <- location _window
          setHref
            "http://127.0.0.1:4433/self-service/registration/browser"
            _location

render :: forall i o m. Unit -> H.ComponentHTML i o m
render _ = HH.div_ []
