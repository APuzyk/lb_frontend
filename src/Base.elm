module Base exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import User exposing (User)
import Json.Decode exposing (bool)

createEntryUrl : String
createEntryUrl = 
    "/create_entry"


viewNavbar : User -> Html msg
viewNavbar user = 
    let 
        userElementClasses : String
        userElementClasses =
            if String.length user.accessToken > 0 then
                "navbar-brand"
            else
                "d-none" 
    in
        nav [ class "navbar navbar-expand-lg navbar-light bg-light shadow" ] [
            div [class "container-fluid" ] [
                a [ class "navbar-brand", href "" ] [ text "My Journal" ],
                a [ class userElementClasses, href createEntryUrl ] [ text "Write Entry" ]
            ]
        ]

viewFooter : Html msg
viewFooter =
    footer [ class "py-3 bg-grey" ]
        [ 
            p [ class "m-0 text-dark text-center " ] [ text ("Copyright " ++ (String.fromChar (Char.fromCode 169)) ++ " Leatherbound")]
        ]