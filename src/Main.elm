module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing ( onInput, onClick )
import Http
import Url
import Base as B
import Json.Decode as Decode exposing (..)
import Json.Encode as Encode exposing (..)
import Html.Events exposing (onClick)
import User as U
import EntryView as EV
import Urls exposing (api, registerUrl)
import Types exposing (Msg, Model)
import Types as T
import Auth as A
import Entry exposing (Entries)
import EntryPull as EP



-- MAIN
main : Program () Model Msg
main =
  Browser.application
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    , onUrlChange = T.UrlChanged
    , onUrlRequest = T.LinkClicked
    }

init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
  let
    newUser = U.User "" "" "" ""
    entries = Entries "" []
  in
    ( Model key url newUser "" entries, Cmd.none )



-- UPDATE
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    T.LinkClicked urlRequest ->
      case urlRequest of
        Browser.Internal url ->
          ( model, Nav.pushUrl model.key (Url.toString url) )

        Browser.External href ->
          ( model, Nav.load href )

    T.UrlChanged url ->
        let
            newModel = { model | url = url }
        in
            updateUrlAction newModel url
    
    T.SetUsername username ->
        let 
            oldUser = model.user
            newUser = { oldUser | username = username}
        in
            ( { model | user = newUser }, Cmd.none )

    T.SetPassword password ->
        let
            oldUser = model.user
            newUser = { oldUser | password = password }
        in
            ( { model | user = newUser }, Cmd.none )

    T.ClickRegisterUser ->
        ( model, A.authUser model )

    T.GotToken result ->
        A.getTokenCompleted model result
    
    T.GotEntries result ->
        EP.getEntriesCompleted model result

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none



-- VIEW
view : Model -> Browser.Document Msg
view model =
    { title = "Leatherbound"
    , body =
        [ B.viewNavbar model.user
        , div [ class "container" ]
            [ h2 [ class "text-center" ] [ text "Welcome" ]
            , div [ class "jumbotron text-left" ]
                [ -- Login/Register form or user greeting
                  A.authBoxView model.user model.errorMsg,
                  EV.viewEntryCards model.entries
                ]
            ]
         , B.viewFooter
      ]
    }

updateUrlAction : Model -> Url.Url -> (Model, Cmd Msg)
updateUrlAction model url =
    case url.path of
        "/"-> (model, EP.getEntries model)
        _ -> ( model, Cmd.none ) 
