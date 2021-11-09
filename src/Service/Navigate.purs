module Service.Navigate where

import Prelude (class Monad, Unit, (<<<))
import Service.Route (Route)
import Halogen
import Routing.PushState (LocationState)

class
  Monad m <=
  Navigate m where
  navigate :: Route -> m Unit
  locationState :: m LocationState

instance navigateHalogenM :: Navigate m => Navigate (HalogenM state action slots msg m) where
  navigate = lift <<< navigate
  locationState = lift locationState
