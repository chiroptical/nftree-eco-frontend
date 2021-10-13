module Main where

import Prelude (Unit, Void, bind, void, when, ($), (/=), discard, (<>), show, (>>=), pure)
import Effect.Class.Console (log)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Either (hush)
-- Web
import Web.HTML (window)
import Web.HTML.Location as Location
import Web.HTML.Window as Window
-- Internal Components
import AppM (runAppM)
import Env (Env)
import Component.Router as Router
-- Internal Service
import Service.Route (Route(..), routeCodec)
-- Effects
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Aff (Aff, launchAff_)
-- Routing
import Routing.Duplex (parse)
import Routing.PushState (makeInterface, matchesWith)
-- Halogen
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

main :: Effect Unit
main =
  HA.runHalogenAff do
    body <- HA.awaitBody
    nav <- liftEffect makeInterface
    let
      environment :: Env
      environment = { nav }

      rootComponent :: H.Component Router.Query Route Void Aff
      rootComponent = H.hoist (runAppM environment) Router.component
    { _pathname, _search } <-
      liftEffect
        $ do
            _location <- window >>= Window.location
            _pathname <- Location.pathname _location
            _search <- Location.search _location
            pure { _pathname, _search }
    log $ "Main _pathname " <> show _pathname
    log $ "Main _search " <> show _search
    let
      route = fromMaybe Home (hush $ parse routeCodec (_pathname <> _search))
    halogenIO <- runUI rootComponent route body
    let
      onRouteChange :: Maybe Route -> Route -> Effect Unit
      onRouteChange old new =
        when (old /= Just new) do
          log $ show old <> " -> " <> show new
          launchAff_ $ halogenIO.query $ H.mkTell $ Router.Navigate new
    void $ liftEffect $ matchesWith (parse routeCodec) onRouteChange nav
