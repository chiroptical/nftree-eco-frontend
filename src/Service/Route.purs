module Service.Route where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Routing.Duplex (RouteDuplex', path, root)
import Routing.Duplex.Generic (noArgs, sum)

data Route
  = Home
  | Register
  | Login

derive instance genericRoute :: Generic Route _

derive instance eqRoute :: Eq Route

derive instance ordRoute :: Ord Route

instance showRoute :: Show Route where
  show = genericShow

routeCodec :: RouteDuplex' Route
routeCodec =
  root
    $ sum
        { "Home": noArgs
        , "Register": path "register" noArgs
        , "Login": path "login" noArgs
        }
