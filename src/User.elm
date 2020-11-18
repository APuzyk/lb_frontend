module User exposing (..)

import Http exposing (header)
import Html.Attributes exposing (value)
import Url exposing (Url)
import Json.Encode as Encode
import Task exposing (Task)

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