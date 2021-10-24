module Data.User where

import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CAR

type User
  = { username :: String, email :: String, password :: String }

userCodec :: CA.JsonCodec User
userCodec =
  CA.object "User"
    ( CAR.record
        { username: CA.string
        , email: CA.string
        , password: CA.string
        }
    )
