module Env where

import Routing.PushState (PushStateInterface)

type Env
  = { nav :: PushStateInterface
    }
