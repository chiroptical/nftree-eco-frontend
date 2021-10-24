module Component.Router where

import Data.Either (hush)
import Data.Maybe (Maybe(..), fromMaybe)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Page.Home as Home
import Page.Login as Login
import Page.Register as Register
import Prelude (Unit, bind, discard, pure, show, unit, when, ($), (/=), (<>))
import Routing.Duplex (parse)
import Service.Navigate (class Navigate, navigate, locationState)
import Service.Route (Route(..), routeCodec)
import Web.Event.Event (preventDefault)
import Web.UIEvent.MouseEvent (toEvent, MouseEvent)

type Input
  = Route

type State
  = { route :: Route
    }

data Action
  = Initialize
  | GoTo Route MouseEvent

data Query a
  = Navigate Route a

type ChildSlots
  = ( home :: Home.Slot Unit
    , register :: Register.Slot Unit
    , login :: Login.Slot Unit
    )

component ::
  forall o m.
  MonadEffect m =>
  MonadAff m =>
  Navigate m =>
  H.Component Query Input o m
component =
  H.mkComponent
    { initialState: \initialRoute -> { route: initialRoute }
    , render
    , eval:
        H.mkEval
          $ H.defaultEval
              { handleAction = handleAction
              , handleQuery = handleQuery
              , initialize = Just Initialize
              }
    }

-- Renders a page component depending on which route is matched.
render ::
  forall m.
  MonadEffect m =>
  MonadAff m =>
  Navigate m =>
  State ->
  H.ComponentHTML Action ChildSlots m
render { route } =
  navbar
    $ case route of
        Home -> HH.slot_ Home._home unit Home.component unit
        Register -> HH.slot_ Register._register unit Register.component {}
        Login -> HH.slot_ Login._login unit Login.component { statusCode: Nothing }

handleAction ::
  forall o m.
  MonadEffect m =>
  Navigate m =>
  Action ->
  H.HalogenM State Action ChildSlots o m Unit
handleAction = case _ of
  Initialize -> do
    currentRoute <- H.gets _.route
    mNextRoute <- do
      { pathname, search } <- locationState
      let
        result = parse routeCodec (pathname <> search)
      pure $ hush result
    when (Just currentRoute /= mNextRoute)
      $ do
          log $ "Initialize, currentRoute: " <> show currentRoute <> " mNextRoute: " <> show mNextRoute
          navigate $ fromMaybe Home mNextRoute
  GoTo new e -> do
    liftEffect $ preventDefault (toEvent e)
    old <- H.gets _.route
    when (old /= new)
      $ do
          log $ "GoTo " <> show new <> " from " <> show old
          navigate new

handleQuery ::
  forall a o m.
  MonadEffect m =>
  Query a ->
  H.HalogenM State Action ChildSlots o m (Maybe a)
handleQuery = case _ of
  Navigate new a -> do
    old <- H.gets _.route
    when (old /= new)
      $ do
          H.modify_ _ { route = new }
          log $ "Navigate to " <> show new <> " from " <> show old
    pure (Just a)

navbar :: forall w. HH.HTML w Action -> HH.HTML w Action
navbar html =
  HH.div_
    [ HH.ul_
        [ HH.li_
            [ HH.a
                [ HP.href "#"
                , HE.onClick (GoTo Home)
                ]
                [ HH.text "Home" ]
            ]
        , HH.li_
            [ HH.a
                [ HP.href "#"
                , HE.onClick $ GoTo Register
                ]
                [ HH.text "Register" ]
            ]
        , HH.li_
            [ HH.a
                [ HP.href "#"
                , HE.onClick $ GoTo Login
                ]
                [ HH.text "Login" ]
            ]
        ]
    , html
    ]
