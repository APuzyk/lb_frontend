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

-- <nav class="navbar navbar-expand-lg navbar-light bg-light shadow" id="mainNav">
--         <div class="container-fluid">
--             <a class="navbar-brand" href="{% url 'home' %}">My Journal</a>
--             {% if user.is_authenticated %}
--             <a class="navbar-brand" href="{% url 'entry_create' %}">Write Entry</a>
--             {% endif %}
--             <button class="navbar-toggler navbar-toggler-right" type="button" data-toggle="collapse"
--                 data-target="#navbarResponsive" aria-controls="navbarResponsive" aria-expanded="false"
--                 aria-label="Toggle navigation">
--                 <span class="navbar-toggler-icon"></span>
--             </button>
--             <div class="collapse navbar-collapse" id="navbarResponsive">
--                 <ul class="navbar-nav ml-auto">
--                     {% if user.is_authenticated %}
--                     <a class="navbar-brand" href="{% url 'logout' %}">Logout</a>
--                     {% else %}
--                     <a class="navbar-brand" href="{% url 'register' %}">Register</a>
--                     <a class="navbar-brand" href="{% url 'login' %}">Login</a>
--                     {% endif %}
--                 </ul>
--             </div>
--         </div>
--     </nav>

viewFooter : Html msg
viewFooter =
    footer [ class "py-3 bg-grey" ]
        [ 
            p [ class "m-0 text-dark text-center " ] [ text ("Copyright " ++ (String.fromChar (Char.fromCode 169)) ++ " Leatherbound")]
        ]