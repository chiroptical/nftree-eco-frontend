module Main where

import AppM (runAppM)
import Component.Router as Router
import Data.Either (hush)
import Data.Maybe (Maybe(..), fromMaybe)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Env (Env)
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Prelude
import Routing.Duplex (parse)
import Routing.PushState (makeInterface, matchesWith)
import Service.Route (Route(..), routeCodec)
import Web.HTML (window)
import Web.HTML.Location as Location
import Web.HTML.Window as Window

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
      parseResult = parse routeCodec (_pathname <> _search)

      route = fromMaybe Home (hush parseResult)
    log $ "parseResult: " <> show parseResult
    halogenIO <- runUI rootComponent route body
    let
      onRouteChange :: Maybe Route -> Route -> Effect Unit
      onRouteChange old new =
        when (old /= Just new) do
          log $ "onRouteChange: " <> show old <> " -> " <> show new
          launchAff_ $ halogenIO.query $ H.mkTell $ Router.Navigate new
    void $ liftEffect $ matchesWith (parse routeCodec) onRouteChange nav
