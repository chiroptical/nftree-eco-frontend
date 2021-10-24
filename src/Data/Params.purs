module Data.Params where

import Data.Maybe (Maybe(..))

-- TODO: string is probably not the right representation here...
type RegistrationParams
  = { username :: Maybe String
    , email :: Maybe String
    , password :: Maybe String
    }

emptyRegistrationParams :: RegistrationParams
emptyRegistrationParams =
  { username: Nothing
  , email: Nothing
  , password: Nothing
  }

-- TODO: string is probably not the right representation here...
type LoginParams
  = { username :: Maybe String
    , password :: Maybe String
    }

emptyLoginParams :: LoginParams
emptyLoginParams =
  { username: Nothing
  , password: Nothing
  }
