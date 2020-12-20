module Auth exposing (..)

import Html exposing (..)

import Html.Attributes exposing (..)
import User exposing (User, userEncoder, emptyUser)
import Types exposing (..)
import Types as T
import Html.Events exposing ( onInput, onClick )
import Http
import Json.Decode as Decode exposing (..)
import Json.Encode as Encode
import Urls exposing (registerUrl)
import Url.Builder as Builder
import Url
import Browser.Navigation as Nav
import Browser
import Base as B
import Urls exposing (createUserUrl, tokenRefreshUrl)
import HttpHelpers exposing (httpErrorToString)
import Urls exposing (api)
import Jwt exposing (getTokenExpirationMillis, JwtError)
import Time exposing (millisToPosix)
import Task
import Process
import Debug




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
                    Cmd.batch [ Nav.pushUrl model.key (Url.toString newUrl)
                              , delayedRefreshCmd model]
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
        
        apiUrl = registerUrl model.url
        
    in
        Http.post 
            { url = apiUrl
            , body = body
            , expect= Http.expectJson GotToken tokenDecoder
            }

viewAuth : Model -> Browser.Document Msg
viewAuth model =
    { title = "Leatherbound"
    , body =
        [ B.viewNavbar model
        , div [ class "container-fluid" ]
            [
                div [class "row justify-content-center"] [
                    authBoxView model
                ]
            ]
        , B.viewFooter
        ]
    }
authBoxView : Model -> Html Msg
authBoxView model =
    let
    -- If there is an error on authentication, show the error alert
        user = model.user
        errorMsg = Debug.log "test log authboxview:" model.errorMsg
        url = model.url
        
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
                "col-4 justify-content-center"
        colMain = "col-md-8 col-md-offset-6"
        colEntries = "col-md-offset-8 col-md-4"
    in
        div [ id "form", class loggedIn ] [ 
            h2 [ class "text-center" ] [ text "Log In" ],
            div [ class showError ] [ 
                div [ class "alert alert-danger" ] [ text errorMsg ]
            ], 
            div [ class "form-group row justify-content-center" ] [
                    input [ id "username", 
                            type_ "text", 
                            class "form-control", 
                            placeholder "Username",
                            Html.Attributes.value user.username, 
                            onInput SetUsername ] []
            ], 
            div [ class "form-group row justify-content-center" ] [
                    input [ id "password", 
                            type_ "password", 
                            class "form-control",
                            placeholder "Password",
                            Html.Attributes.value user.password, 
                            onInput SetPassword ] []
            ], 
            div [ class "text-center" ] [ 
                button [ class "btn btn-link", onClick ClickRegisterUser ] [ text "Login" ]
                ],
            div [ class "text-center" ] [ 
                a [ class "btn btn-link", href (Urls.getRegisterPath url)] [ text "Register" ]
                ]
            ]

-- REFRESH TOKEN ATTEMPT

second : Int
second =
    1000

delayedRefreshCmd : Model -> Cmd Msg
delayedRefreshCmd model =
    let
        user = model.user
        token = Debug.log "token: " user.accessToken
    in 
        tokenExpiryTask (getTokenExpirationMillis token)
            |> Task.attempt (\_ -> AttemptRefreshToken)


{-| A delay task that should end 30 seconds before the token is due to expire.
If the token expiry is less than 1 minute away, the delay is set to half of the remaining
time, which should be under 30 seconds.
The delay will expire immediately if the token expiry is already in the past.
-}
tokenExpiryTask : Result JwtError Int -> Task.Task Never ()
tokenExpiryTask timeoutResult =
    case timeoutResult of 
        Ok timeoutInt ->
            let
                timeout = millisToPosix (Debug.log " timeout result: " timeoutInt)
                safeInterval =
                    30 * second

                delay posixBy posixNow =
                    let
                        by =
                            Time.posixToMillis posixBy

                        now =
                            Time.posixToMillis posixNow
                    in
                        Basics.max ((by - now) // 2) (by - now - safeInterval) |> Basics.max 0
            in
                Time.now |> Task.andThen (\now -> toFloat (delay timeout now) |> Process.sleep)
        Err error ->
            Process.sleep 0



attemptRefreshToken : Model -> Cmd Msg
attemptRefreshToken model =
    let
        body = 
            model.user
                |> \user -> Encode.object [ ("refresh", Encode.string user.refreshToken)]
                |> Http.jsonBody
        
        apiUrl = Debug.log "test log" (tokenRefreshUrl model.url)

    in
        Http.post
            { url = apiUrl
            , body = body
            , expect = Http.expectJson GotAccessToken (Decode.field "access" Decode.string) }

attemptRefreshTokenCompleted : Model -> Result Http.Error String -> ( Model, Cmd Msg )
attemptRefreshTokenCompleted model result =
    case result of
        Ok newToken ->
            let 
                oldUser = model.user
                newUser = {oldUser | password = "", accessToken = newToken }
                newModel = { model | user = newUser, errorMsg = "" }

            in
                ( 
                    newModel, 
                    delayedRefreshCmd newModel
                )
        
        Err error ->
            let 
                newUser = emptyUser
                oldUrl = model.url
                newUrl = { oldUrl | path = "", query = Nothing, fragment = Nothing}
            in
                ( 
                    { model | user = newUser, errorMsg = "Your Session Expired, Please Log Back In" }, 
                    Nav.pushUrl model.key (Url.toString newUrl)
                )