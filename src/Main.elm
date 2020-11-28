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
import User as U
import EntryView as EV
import Urls exposing (api, registerUrl)
import Types exposing (Msg, Model)
import Types as T
import Auth as A
import Entry exposing (Entries)
import EntryPull as EP
import Dict
import Url.Parser as UrlParser
import EntryUpdate as EU
import EntryDelete as ED
import EntryCreate as EC
import UserCreate as UC




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
    newUser = U.User "" "" "" "" ""
    entries = Entries "" Dict.empty Nothing False
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
    T.SetPasswordAgain passwordAgain ->
        let
            oldUser = model.user
            newUser = { oldUser | passwordAgain = passwordAgain}
        in
            ({model | user = newUser}, Cmd.none)

    T.ClickRegisterUser ->
        ( model, A.authUser model )

    T.GotToken result ->
        A.getTokenCompleted model result
    
    T.GotEntries result ->
        EP.getEntriesCompleted model result
    
    T.UpdateEntryContent content ->
        let
            entries = model.entries
            activeEntry = entries.activeEntry
        in
            case activeEntry of
                Just entry ->
                    ( { model | entries = { entries | activeEntry = Just {entry | content = content}}}, Cmd.none)
                Nothing ->
                    (model, Cmd.none)
    
    T.UpdateEntryTitle title ->
        let
            entries = model.entries
            activeEntry = entries.activeEntry
        in
            case activeEntry of
                Just entry ->
                    ({ model | entries = { entries | activeEntry = Just {entry | title = title}}}, Cmd.none)
                Nothing ->
                    (model, Cmd.none)
    T.ClickSaveEntry ->
        let
            entries = model.entries
            activeEntry = entries.activeEntry
        in
            case activeEntry of
                Just entry ->
                    ( model, EU.updateEntry model entry )
                Nothing ->
                    (model, Cmd.none)

    T.PatchedEntry result ->
        EU.patchedEntryCompleted model result
    
    T.ClickDeleteEntry entry ->
        let
            entries = model.entries
            newEntries = {entries | activeEntry = Just entry }
            newModel = { model | entries = newEntries }
        in
            ( newModel , ED.deleteEntry newModel entry )
    
    T.DeletedEntry result ->
        ED.deletedEntryCompleted model result
    
    T.ClickCreateEntry ->
        let
            entries = model.entries
            activeEntry = entries.activeEntry
        in
            case activeEntry of
                Just entry ->
                    (model, EC.createEntry model entry)
                Nothing ->
                    (model, Cmd.none)
    
    T.CreatedEntry result ->
        EU.patchedEntryCompleted model result
    
    T.ClickCreateUser ->
        (model, UC.createUser model)
    
    T.CreatedUser result ->
        UC.createdUserCompleted model result


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none



-- VIEW
view : Model -> Browser.Document Msg
view model =
    let
        user = model.user

    in
        case user.accessToken of
            "" ->
                A.viewAuth model
            "reg" ->
                UC.viewCreateUser model
            _ ->
                viewEntries model

updateUrlAction : Model -> Url.Url -> (Model, Cmd Msg)
updateUrlAction model url =
    let
        oldEntries = model.entries
    in
        case UrlParser.parse T.routeParser url of
            Nothing -> 
                ({model | entries = removeActiveEntry model.entries} , EP.getEntries model)
            Just (T.EntryRoute created_on uuid) -> 
                ( {model | entries = updateActiveEntry created_on uuid model.entries} , Cmd.none )
            Just (T.EditEntryRoute created_on uuid) -> 
                ( {model | entries = updateEditActiveEntry created_on uuid model.entries} , Cmd.none )
            Just T.CreateEntryRoute ->
                ( {model | entries = addEmtpyActiveEntry model.entries }, Cmd.none )
            Just T.CreateUserRoute ->
                let
                    user = model.user
                in
                ( { model | user = {user | accessToken = "reg"}}, Cmd.none)

removeActiveEntry : Entries -> Entries
removeActiveEntry entries =
    {entries | activeEntry = Nothing }

updateActiveEntry : String -> String -> Entries -> Entries
updateActiveEntry created_on uuid entries =
    {entries | activeEntry = (Dict.get (created_on ++ "_" ++ uuid) entries.entries), editable = False }

updateEditActiveEntry : String -> String -> Entries -> Entries
updateEditActiveEntry created_on uuid entries =
    let
       activeEntry = (Dict.get (created_on ++ "_" ++ uuid) entries.entries)
    in
        case activeEntry of
            Nothing -> {entries | activeEntry = Nothing, editable = False }
            Just entry -> {entries | activeEntry = Just entry, editable = True} 

addEmtpyActiveEntry : Entries -> Entries
addEmtpyActiveEntry entries =
    {entries | activeEntry = Just (Entry.Entry "" "" "" "" "" ""), editable = True }

        


viewEntries : Model -> Browser.Document Msg
viewEntries model = 
    let
        entries = model.entries
    in
        case entries.activeEntry of
            Nothing -> 
                { title = "Leatherbound"
                        , body =
                            [ B.viewNavbar model
                            , div [ class "container-fluid" ]
                                [
                                    div [class "row"] [
                                    EV.viewEntriesSidebar model,
                                    EV.viewEntryCards model
                                    
                                ]   
                                ]
                            , B.viewFooter
                        ]
                }
            Just entry ->
                { title = "Leatherbound"
                        , body =
                            [ B.viewNavbar model
                            , div [ class "container-fluid" ]
                                [
                                    div [class "row"] [
                                    EV.viewEntriesSidebar model,
                                    if entry.id == "" then
                                        EV.viewCreateEntry model
                                    else
                                        if entries.editable then
                                            EV.viewEditableEntry model.url entry
                                        else    
                                            EV.viewSingleEntry model.url entry
                                ]   
                                ]
                            , B.viewFooter
                        ]
                }


