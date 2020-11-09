module Auth exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import User exposing (User, userEncoder)
import Types exposing (..)
import Types as T
import Html.Events exposing ( onInput, onClick )
import Http
import Json.Decode as Decode exposing (..)
import Urls exposing (registerUrl)
import Url.Builder as Builder
import Url
import Browser.Navigation as Nav





getTokenCompleted :  Model -> Result Http.Error TokenStrings -> ( Model, Cmd Msg )
getTokenCompleted model result =
    case result of
        Ok newToken ->
            let 
                newUser = updateTokenValues newToken model.user
                oldUrl = model.url
                newUrl = { oldUrl | path = "", query = Nothing, fragment = Nothing}
            in
                ( 
                    { model | user = newUser, errorMsg = "" }, 
                    Nav.pushUrl model.key (Url.toString newUrl)
                )
        
        Err error ->
            ( { model | errorMsg = (httpErrorToString error) }, Cmd.none )


updateTokenValues : TokenStrings -> User -> User
updateTokenValues tokens user =
    {user | password = "", refreshToken = tokens.refreshToken, accessToken = tokens.accessToken }

--post register/login
tokenDecoder : Decoder TokenStrings
tokenDecoder =
    map2 TokenStrings
        (Decode.field "refresh" Decode.string)
        (Decode.field "access" Decode.string)


authUser : Model -> Cmd Msg
authUser model = 
    let
        body = 
            model.user
                |> userEncoder
                |> Http.jsonBody
        
        apiUrl = registerUrl
        
    in
        Http.post 
            { url = apiUrl
            , body = body
            , expect= Http.expectJson GotToken tokenDecoder
            }


httpErrorToString : Http.Error -> String
httpErrorToString error =
    case error of
        Http.BadUrl url ->
            "The URL " ++ url ++ " was invalid"
        Http.Timeout ->
            "Unable to reach the server, try again"
        Http.NetworkError ->
            "Unable to reach the server, check your network connection"
        Http.BadStatus 500 ->
            "The server had a problem, try again later"
        Http.BadStatus 400 ->
            "Verify your information and try again"
        Http.BadStatus 401 ->
            "Incorrect Username or Password"
        Http.BadStatus _ ->
            "Unknown error"
        Http.BadBody errorMessage ->
            errorMessage


authBoxView : User -> String -> Html Msg
authBoxView user errorMsg =
    let
    -- If there is an error on authentication, show the error alert
                showError : String
                showError =
                    if String.length errorMsg == 0 then
                        "d-none"
                    else
                        ""

                -- Greet a logged in user by username
                loggedIn : String
                loggedIn =
                    if String.length user.accessToken > 0 then
                        "d-none"
                    else
                        ""
    in
        div [ id "form", class loggedIn ] [ 
            h2 [ class "text-center" ] [ text "Log In or Register" ],
            p [ class "help-block" ] [ text "If you already have an account, please Log In. Otherwise, enter your desired username and password and Register." ],
            div [ class showError ] [ 
                div [ class "alert alert-danger" ] [ text errorMsg ]
            ], 
            div [ class "form-group row" ] [
                div [ class "col-md-offset-2 col-md-8" ] [ 
                    label [ for "username" ] [ text "Username:" ],
                    input [ id "username", type_ "text", class "form-control", Html.Attributes.value user.username, onInput SetUsername ] []
                ]
            ], 
            div [ class "form-group row" ] [
                div [ class "col-md-offset-2 col-md-8" ] [ 
                    label [ for "password" ] [ text "Password:" ],
                    input [ id "password", 
                            type_ "password", 
                            class "form-control", 
                            Html.Attributes.value user.password, onInput SetPassword ] []
                ]
            ], 
            div [ class "text-center" ] [ 
                button [ class "btn btn-link", onClick ClickRegisterUser ] [ text "Login" ]
                ]
            ]