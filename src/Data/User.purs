module Data.User where

import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CAR

type User
  = { email :: String
    , password :: String
    }

userCodec :: CA.JsonCodec User
userCodec =
  CA.object "User"
    ( CAR.record
        { email: CA.string
        , password: CA.string
        }
    )
