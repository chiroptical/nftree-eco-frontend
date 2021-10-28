module Request where

import Prelude
import Data.Argonaut.Core as A
import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Either (Either)
import Effect.Aff (Aff, Error)
import Effect.Aff as Aff
import Milkis as M
import Milkis.Impl.Window (windowFetch)

data BackendRoute
  = AuthRegister
  | AuthLogin

renderBackendRoute :: BackendRoute -> M.URL
renderBackendRoute =
  -- TODO: host should be in an Environment
  let
    host = "http://localhost:3001/"
  in
    M.URL
      <<< case _ of
          AuthRegister -> host <> "auth/register"
          AuthLogin -> host <> "auth/login"

fetch = M.fetch windowFetch

post :: forall a. BackendRoute -> JsonCodec a -> a -> Aff (Either Error M.Response)
post route codec obj =
  Aff.attempt
    $ fetch
        (renderBackendRoute route)
        { method: M.postMethod
        , credentials: M.includeCredentials
        , body:
            A.stringify $ CA.encode codec obj
        , headers:
            M.makeHeaders
              { "Content-Type": "application/json"
              }
        }
