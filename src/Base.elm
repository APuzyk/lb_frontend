module Base exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import User exposing (User)
import Json.Decode exposing (bool)
import Urls
import Urls exposing (getBasePath, createEntryUrl, insightsUrl)
import Types exposing (Model)

viewNavbar : Model -> Html msg
viewNavbar model = 
    let
        user = model.user
        url = model.url 
        userElementList : List(Html msg)
        userElementList =
            if String.length user.accessToken > 0 then
                [
                    li [class "nav-item"] [a [ class "nav-link", href (getBasePath url) ] [ text "My Journal" ]],
                    li [class "nav-item"] [a [ class "nav-link", href (createEntryUrl url) ] [ text "Write Entry"]],
                    li[class "nav-item"] [a [ class "nav-link", href (insightsUrl url) ] [ text "Insights" ]]
                ]
            else
                []
    in
        nav [ class "navbar navbar-expand-lg navbar-light bg-light shadow" ] [
            div [class "container-fluid" ] [
                div [class "navbar-brand"] [text "Leatherbound"],
                ul [class "nav navbar-nav mr-auto"] userElementList
            ]
        ]

viewFooter : Html msg
viewFooter =
    footer [ class "py-3 bg-grey" ]
        [ 
            p [ class "m-0 text-dark text-center " ] [ text ("Copyright " ++ (String.fromChar (Char.fromCode 169)) ++ " Leatherbound")]
        ]
