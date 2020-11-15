module EntryDelete exposing (..)

import Http
import Json.Encode as Encode
import Types exposing (Model, Msg)
import Entry exposing (Entry, Entries)
import EntryPull as EP
import EntryView exposing (getEntryHref)
import Dict
import Auth exposing (httpErrorToString)
import Browser.Navigation as Nav
import Urls
import Jwt.Http as JHttp
import Urls exposing (basePath)

deleteEntry : Model -> Entry -> Cmd Msg
deleteEntry model entry = 
    let
        entries = model.entries
        user = model.user
        apiUrl = Urls.api ++ "entry/" ++ entry.id ++ "/"
        
    in
        JHttp.delete
            user.accessToken
            { url = apiUrl
            , expect= Http.expectWhatever Types.DeletedEntry
            }

deletedEntryCompleted : Model -> Result Http.Error () -> ( Model, Cmd Msg )
deletedEntryCompleted model result =
    case result of
        Ok entry ->
            let 
                entries = model.entries
                newEntries = removeActiveEntry entries
            in
                ( { model | entries = newEntries, errorMsg = "" }, 
                Nav.pushUrl model.key (basePath))
        
        Err (Http.BadStatus 204) ->
            let
                entries = model.entries
                newEntries = removeActiveEntry entries
            in
                ( { model | entries = newEntries, errorMsg = "" },
                    Nav.pushUrl model.key (basePath))
        
        Err error ->
            ( { model | errorMsg = (httpErrorToString error) }, Cmd.none )

removeActiveEntry : Entries -> Entries
removeActiveEntry entries =
    let
        activeEntry = entries.activeEntry    
    in
        case activeEntry of
            Just entry ->
                {entries | entries = Dict.remove (EP.genEntryKey entry) entries.entries,
                           activeEntry = Nothing,
                           editable = False}
            Nothing ->
                {entries | editable = False}

    