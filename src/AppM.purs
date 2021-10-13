module AppM where

import Prelude (class Applicative, class Apply, class Bind, class Functor, class Monad, type (~>), bind, ($), (<$>), (=<<))
import Control.Monad.Reader.Trans (class MonadAsk, ReaderT, ask, asks, runReaderT)
import Type.Equality (class TypeEquals, from)
-- Internal Service
import Service.Navigate (class Navigate, locationState)
import Service.Route (routeCodec)
-- Effect
import Effect.Class (class MonadEffect, liftEffect)
-- Aff
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
-- Routing
import Routing.Duplex (print)
-- Env
import Env (Env)

newtype AppM a
  = AppM (ReaderT Env Aff a)

runAppM :: Env -> AppM ~> Aff
runAppM env (AppM m) = runReaderT m env

derive newtype instance functorAppM :: Functor AppM

derive newtype instance applyAppM :: Apply AppM

derive newtype instance applicativeAppM :: Applicative AppM

derive newtype instance bindAppM :: Bind AppM

derive newtype instance monadAppM :: Monad AppM

derive newtype instance monadEffectAppM :: MonadEffect AppM

derive newtype instance monadAffAppM :: MonadAff AppM

instance monadAskAppM :: TypeEquals e Env => MonadAsk e AppM where
  ask = AppM $ asks from

instance navigateAppM :: Navigate AppM where
  navigate route = do
    { pushState } <- asks _.nav
    { state } <- locationState
    liftEffect $ pushState state $ print routeCodec $ route
  locationState = liftEffect =<< _.nav.locationState <$> ask
