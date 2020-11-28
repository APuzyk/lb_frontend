module User exposing (..)

import Json.Encode as Encode

type alias User = 
    {username: String
    , password: String
    , accessToken: String
    , refreshToken: String
    , passwordAgain: String
    }


userEncoder : User -> Encode.Value
userEncoder user =
    Encode.object
        [ ("username", Encode.string user.username)
        , ("password", Encode.string user.password)
        ]     