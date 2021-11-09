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
import Prelude (Unit, bind, discard, pure, show, unit, when, ($), (/=), (<>), negate)
import Routing.Duplex (parse)
import Service.Navigate (class Navigate, navigate, locationState)
import Service.Route (Route(..), routeCodec)
import Web.Event.Event (preventDefault)
import Web.UIEvent.MouseEvent (toEvent, MouseEvent)
import Tailwind as T
import Halogen.HTML.Properties.ARIA as HPA

type Input
  = Route

type State
  =
  { route :: Route
  }

data Action
  = Initialize
  | GoTo Route MouseEvent

data Query a
  = Navigate Route a

type ChildSlots
  =
  ( home :: Home.Slot Unit
  , register :: Register.Slot Unit
  , login :: Login.Slot Unit
  )

component
  :: forall o m
   . MonadEffect m
  => MonadAff m
  => Navigate m
  => H.Component Query Input o m
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
render
  :: forall m
   . MonadEffect m
  => MonadAff m
  => Navigate m
  => State
  -> H.ComponentHTML Action ChildSlots m
render { route } =
  navbar
    $ case route of
        Home -> HH.slot_ Home._home unit Home.component unit
        Register -> HH.slot_ Register._register unit Register.component { registrationError: Nothing }
        Login -> HH.slot_ Login._login unit Login.component { loginError: Nothing }

handleAction
  :: forall o m
   . MonadEffect m
  => Navigate m
  => Action
  -> H.HalogenM State Action ChildSlots o m Unit
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

handleQuery
  :: forall a o m
   . MonadEffect m
  => Query a
  -> H.HalogenM State Action ChildSlots o m (Maybe a)
handleQuery = case _ of
  Navigate new a -> do
    old <- H.gets _.route
    when (old /= new)
      $ do
          H.modify_ _ { route = new }
          log $ "Navigate to " <> show new <> " from " <> show old
    pure (Just a)

navbar' :: forall w. HH.HTML w Action -> HH.HTML w Action
navbar' html =
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

navbar :: forall w. HH.HTML w Action -> HH.HTML w Action
navbar html =
  HH.div_
    [ HH.nav
        [ HP.classes [ T.bgWhite, T.shadow ] ]
        [ HH.div
            [ HP.classes [ T.maxW7xl, T.mxAuto, T.px2, T.smPx6, T.lgPx8 ] ]
            [ HH.div
                [ HP.classes [ T.relative, T.flex, T.justifyBetween, T.h16 ] ]
                [ HH.div
                    [ HP.classes
                        [ T.absolute
                        , T.insetY0
                        , T.left0
                        , T.flex
                        , T.itemsCenter
                        , T.smHidden
                        ]
                    ]
                    [ --  Mobile menu button 
                      HH.button
                        [ HPA.expanded "false"
                        , HPA.controls "mobile-menu"
                        , HP.classes
                            [ T.inlineFlex
                            , T.itemsCenter
                            , T.justifyCenter
                            , T.p2
                            , T.roundedMd
                            , T.textGray400
                            , T.hoverTextGray500
                            , T.hoverBgGray100
                            , T.focusOutlineNone
                            , T.focusRing2
                            , T.focusRingInset
                            , T.focusRingIndigo500
                            ]
                        , HP.type_ HP.ButtonButton
                        ]
                        [ HH.span
                            [ HP.classes [ T.srOnly ] ]
                            [ HH.text "Open main menu" ]
                        ]
                    ]
                , HH.div
                    [ HP.classes
                        [ T.flex1
                        , T.flex
                        , T.itemsCenter
                        , T.justifyCenter
                        , T.smItemsStretch
                        , T.smJustifyStart
                        ]
                    ]
                    [ HH.div
                        [ HP.classes [ T.flexShrink0, T.flex, T.itemsCenter ] ]
                        [ HH.img
                            [ HP.alt "Workflow"
                            , HP.src "https://tailwindui.com/img/logos/workflow-mark-indigo-600.svg"
                            , HP.classes [ T.block, T.lgHidden, T.h8, T.wAuto ]
                            ]
                        , HH.img
                            [ HP.alt "Workflow"
                            , HP.src "https://tailwindui.com/img/logos/workflow-logo-indigo-600-mark-gray-800-text.svg"
                            , HP.classes [ T.hidden, T.lgBlock, T.h8, T.wAuto ]
                            ]
                        ]
                    , HH.div
                        [ HP.classes [ T.hidden, T.smMl6, T.smFlex, T.smSpaceX8 ] ]
                        [ --  Current: "border-indigo-500 text-gray-900", Default: "border-transparent text-gray-500 hover:border-gray-300 hover:text-gray-700" 
                          HH.a
                            [ HP.classes
                                [ T.borderIndigo500
                                , T.textGray900
                                , T.inlineFlex
                                , T.itemsCenter
                                , T.px1
                                , T.pt1
                                , T.borderB2
                                , T.textSm
                                , T.fontMedium
                                ]
                            , HP.href "#"
                            ]
                            [ HH.text "Dashboard" ]
                        , HH.a
                            [ HP.classes
                                [ T.borderTransparent
                                , T.textGray500
                                , T.hoverBorderGray300
                                , T.hoverTextGray700
                                , T.inlineFlex
                                , T.itemsCenter
                                , T.px1
                                , T.pt1
                                , T.borderB2
                                , T.textSm
                                , T.fontMedium
                                ]
                            , HP.href "#"
                            ]
                            [ HH.text "Team" ]
                        , HH.a
                            [ HP.classes
                                [ T.borderTransparent
                                , T.textGray500
                                , T.hoverBorderGray300
                                , T.hoverTextGray700
                                , T.inlineFlex
                                , T.itemsCenter
                                , T.px1
                                , T.pt1
                                , T.borderB2
                                , T.textSm
                                , T.fontMedium
                                ]
                            , HP.href "#"
                            ]
                            [ HH.text "Projects" ]
                        , HH.a
                            [ HP.classes
                                [ T.borderTransparent
                                , T.textGray500
                                , T.hoverBorderGray300
                                , T.hoverTextGray700
                                , T.inlineFlex
                                , T.itemsCenter
                                , T.px1
                                , T.pt1
                                , T.borderB2
                                , T.textSm
                                , T.fontMedium
                                ]
                            , HP.href "#"
                            ]
                            [ HH.text "Calendar" ]
                        ]
                    ]
                , HH.div
                    [ HP.classes
                        [ T.absolute
                        , T.insetY0
                        , T.right0
                        , T.flex
                        , T.itemsCenter
                        , T.pr2
                        , T.smStatic
                        , T.smInsetAuto
                        , T.smMl6
                        , T.smPr0
                        ]
                    ]
                    [ HH.button
                        [ HP.classes
                            [ T.bgWhite
                            , T.p1
                            , T.roundedFull
                            , T.textGray400
                            , T.hoverTextGray500
                            , T.focusOutlineNone
                            , T.focusRing2
                            , T.focusRingOffset2
                            , T.focusRingIndigo500
                            ]
                        , HP.type_ HP.ButtonButton
                        ]
                        [ HH.span
                            [ HP.classes [ T.srOnly ] ]
                            [ HH.text "View notifications" ]
                        ]
                    , --  Profile dropdown 
                      HH.div
                        [ HP.classes [ T.ml3, T.relative ] ]
                        [ HH.div
                            []
                            [ HH.button
                                [ HPA.hasPopup "true"
                                , HPA.expanded "false"
                                , HP.id "user-menu-button"
                                , HP.classes
                                    [ T.bgWhite
                                    , T.roundedFull
                                    , T.flex
                                    , T.textSm
                                    , T.focusOutlineNone
                                    , T.focusRing2
                                    , T.focusRingOffset2
                                    , T.focusRingIndigo500
                                    ]
                                , HP.type_ HP.ButtonButton
                                ]
                                [ HH.span
                                    [ HP.classes [ T.srOnly ] ]
                                    [ HH.text "Open user menu" ]
                                , HH.img
                                    [ HP.alt ""
                                    , HP.src "https://images.unsplash.com/photo-1472099645785-5658abf4ff4e?ixlib=rb-1.2.1&ixid=eyJhcHBfaWQiOjEyMDd9&auto=format&fit=facearea&facepad=2&w=256&h=256&q=80"
                                    , HP.classes [ T.h8, T.w8, T.roundedFull ]
                                    ]
                                ]
                            ]
                        , --             Dropdown menu, show/hide based on menu state.
                          -- 
                          --             Entering: "transition ease-out duration-200"
                          --               From: "transform opacity-0 scale-95"
                          --               To: "transform opacity-100 scale-100"
                          --             Leaving: "transition ease-in duration-75"
                          --               From: "transform opacity-100 scale-100"
                          --               To: "transform opacity-0 scale-95"

                          HH.div
                            [ HP.tabIndex (negate 1)
                            , HPA.labelledBy "user-menu-button"
                            , HPA.orientation "vertical"
                            , HPA.role "menu"
                            , HP.classes
                                [ T.originTopRight
                                , T.absolute
                                , T.right0
                                , T.mt2
                                , T.w48
                                , T.roundedMd
                                , T.shadowLg
                                , T.py1
                                , T.bgWhite
                                , T.ring1
                                , T.ringBlack
                                , T.ringOpacity5
                                , T.focusOutlineNone
                                ]
                            ]
                            [ --  Active: "bg-gray-100", Not Active: "" 
                              HH.a
                                [ HP.id "user-menu-item-0"
                                , HP.tabIndex (negate 1)
                                , HPA.role "menuitem"
                                , HP.classes [ T.block, T.px4, T.py2, T.textSm, T.textGray700 ]
                                , HP.href "#"
                                ]
                                [ HH.text "Your Profile" ]
                            , HH.a
                                [ HP.id "user-menu-item-1"
                                , HP.tabIndex (negate 1)
                                , HPA.role "menuitem"
                                , HP.classes [ T.block, T.px4, T.py2, T.textSm, T.textGray700 ]
                                , HP.href "#"
                                ]
                                [ HH.text "Settings" ]
                            , HH.a
                                [ HP.id "user-menu-item-2"
                                , HP.tabIndex (negate 1)
                                , HPA.role "menuitem"
                                , HP.classes [ T.block, T.px4, T.py2, T.textSm, T.textGray700 ]
                                , HP.href "#"
                                ]
                                [ HH.text "Sign out" ]
                            ]
                        ]
                    ]
                ]
            ]
        , --  Mobile menu, show/hide based on menu state. 
          HH.div
            [ HP.id "mobile-menu", HP.classes [ T.smHidden ] ]
            [ HH.div
                [ HP.classes [ T.pt2, T.pb4, T.spaceY1 ] ]
                [ --  Current: "bg-indigo-50 border-indigo-500 text-indigo-700", Default: "border-transparent text-gray-500 hover:bg-gray-50 hover:border-gray-300 hover:text-gray-700" 
                  HH.a
                    [ HP.classes
                        [ T.bgIndigo50
                        , T.borderIndigo500
                        , T.textIndigo700
                        , T.block
                        , T.pl3
                        , T.pr4
                        , T.py2
                        , T.borderL4
                        , T.textBase
                        , T.fontMedium
                        ]
                    , HP.href "#"
                    ]
                    [ HH.text "Dashboard" ]
                , HH.a
                    [ HP.classes
                        [ T.borderTransparent
                        , T.textGray500
                        , T.hoverBgGray50
                        , T.hoverBorderGray300
                        , T.hoverTextGray700
                        , T.block
                        , T.pl3
                        , T.pr4
                        , T.py2
                        , T.borderL4
                        , T.textBase
                        , T.fontMedium
                        ]
                    , HP.href "#"
                    ]
                    [ HH.text "Team" ]
                , HH.a
                    [ HP.classes
                        [ T.borderTransparent
                        , T.textGray500
                        , T.hoverBgGray50
                        , T.hoverBorderGray300
                        , T.hoverTextGray700
                        , T.block
                        , T.pl3
                        , T.pr4
                        , T.py2
                        , T.borderL4
                        , T.textBase
                        , T.fontMedium
                        ]
                    , HP.href "#"
                    ]
                    [ HH.text "Projects" ]
                , HH.a
                    [ HP.classes
                        [ T.borderTransparent
                        , T.textGray500
                        , T.hoverBgGray50
                        , T.hoverBorderGray300
                        , T.hoverTextGray700
                        , T.block
                        , T.pl3
                        , T.pr4
                        , T.py2
                        , T.borderL4
                        , T.textBase
                        , T.fontMedium
                        ]
                    , HP.href "#"
                    ]
                    [ HH.text "Calendar" ]
                ]
            ]
        ]
    , html
    ]
