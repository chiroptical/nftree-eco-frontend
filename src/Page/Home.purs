module Page.Home where

import Prelude
import Type.Proxy (Proxy(..))
-- Halogen
import Halogen as H
import Halogen.HTML as HH

type Slot p
  = forall query. H.Slot query Void p

_home :: Proxy "home"
_home = Proxy

component :: forall q i o m. H.Component q i o m
component =
  H.mkComponent
    { initialState: identity
    , render
    , eval: H.mkEval H.defaultEval
    }

render :: forall state action m. state -> H.ComponentHTML action () m
render _ = HH.h1_ [ HH.text "Home Page" ]
