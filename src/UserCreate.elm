module UserCreate exposing (..)

import Html exposing (..)

import Html.Attributes exposing (..)
import User exposing (User, userEncoder)
import Types exposing (..)
import Types as T
import Html.Events exposing ( onInput, onClick )
import Http
import Json.Decode as Decode exposing (..)
import Urls exposing (registerUrl, createUserUrl)
import Url.Builder as Builder
import Url
import Browser.Navigation as Nav
import Browser
import Base as B
import Auth exposing (httpErrorToString, authUser)






createUser : Model -> Cmd Msg
createUser model = 
    let
        body = 
            model.user
                |> userEncoder
                |> Http.jsonBody
        
        apiUrl = createUserUrl model.url
        
    in
        Http.post 
            { url = apiUrl
            , body = body
            , expect= Http.expectJson CreatedUser userRegDecoder
            }

type alias TmpUserContainer =
    { id : Int
    , username : String
    }

userRegDecoder : Decoder {id : Int, username: String}
userRegDecoder =
    map2 TmpUserContainer
        (Decode.field "id" Decode.int)
        (Decode.field "username" Decode.string)


createdUserCompleted :  Model -> Result Http.Error {id : int, username: String} -> ( Model, Cmd Msg )
createdUserCompleted model result =
    let
        oldUser = model.user
    in
        case result of
            Ok newUser ->
                case newUser.username of
                    "A user with that username already exists." ->
                        ({model | errorMsg = "A user with that username alread exists.",
                                  user = {oldUser | passwordAgain = "", password = "", username = ""
                        }}, Cmd.none)
                    _ ->
                        ({model | user = {oldUser | passwordAgain = ""}}, authUser {model | user = {oldUser | passwordAgain = ""}})
            Err error ->
                ( { model | errorMsg = (httpErrorToString error) }, Cmd.none )





viewCreateUser : Model -> Browser.Document Msg
viewCreateUser model =
    { title = "Leatherbound"
    , body =
        [ B.viewNavbar model
        , div [ class "container-fluid" ]
            [
                div [class "row justify-content-center"] [
                    createUserView model.user model.errorMsg
                ]
            ]
        , B.viewFooter
        ]
    }
createUserView : User -> String -> Html Msg
createUserView user errorMsg =
    let
    -- If there is an error on authentication, show the error alert
                showError : String
                showError =
                    if String.length errorMsg == 0 then
                        "d-none"
                    else
                        ""
                
                showPasswordMismatch : String
                showPasswordMismatch =
                    if user.password == user.passwordAgain then
                        "d-none"
                    else
                        ""
                colMain = "col-md-8 col-md-offset-6"
                colEntries = "col-md-offset-8 col-md-4"
    in
        div [ id "form", class "col-4 justify-content-center" ] [ 
            h2 [ class "text-center" ] [ text "Create User" ],
            div [ class showError ] [ 
                div [ class "alert alert-danger" ] [ text errorMsg ]
            ],
            div [ class showPasswordMismatch ] [ 
                div [ class "alert alert-danger" ] [ text "Passwords don't match" ]
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
            div [ class "form-group row justify-content-center" ] [
                    input [ id "password", 
                            type_ "password", 
                            class "form-control",
                            placeholder "Re-Type Password",
                            Html.Attributes.value user.passwordAgain, 
                            onInput SetPasswordAgain ] []
            ], 
            div [ class "text-center" ] [ 
                button [ class "btn btn-link", onClick T.ClickCreateUser ] [ text "Create" ]
                ]
            ]