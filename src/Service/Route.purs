module Service.Route where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Routing.Duplex (RouteDuplex', optional, params, string, path, root)
import Routing.Duplex.Generic (noArgs, sum)
import Data.Params (RegistrationParams, LoginParams)

data Route
  = Home
  | Register RegistrationParams
  | Login LoginParams

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
        , "Register":
            path "register"
              ( params
                  { username: optional <<< string
                  , email: optional <<< string
                  , password: optional <<< string
                  }
              )
        , "Login":
            path "login"
              ( params
                  { username: optional <<< string
                  , password: optional <<< string
                  }
              )
        }
