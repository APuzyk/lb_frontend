module EntryPull exposing (..)

import Types as T exposing (Msg, Model)
import Http
import Jwt.Http
import Urls exposing (entriesUrl)
import Json.Decode as D exposing (..)
import Entry exposing (Entry, Entries)
import Auth exposing (httpErrorToString)


getEntriesCompleted :  Model -> Result Http.Error Entries -> ( Model, Cmd Msg )
getEntriesCompleted model result =
    case result of
        Ok entries ->
            let 
                oldEntries = model.entries
                newEntries = {oldEntries 
                                | next_page = entries.next_page, 
                                  entries = oldEntries.entries ++ entries.entries
                            }
            in
                ( 
                    { model | entries = newEntries, errorMsg = "" }, 
                    Cmd.none
                )
        
        Err error ->
            ( { model | errorMsg = (httpErrorToString error) }, Cmd.none )

getEntries : Model -> Cmd Msg
getEntries model =
    let
        user = model.user
    in
        Jwt.Http.get user.accessToken
            { url = entriesUrl
            , expect = Http.expectJson T.GotEntries entriesDecoder}

entriesDecoder : Decoder Entries
entriesDecoder = 
    map2 Entries
        (field "next" string)
        (field "results" listOfEntriesDecoder)


listOfEntriesDecoder : Decoder (List Entry)
listOfEntriesDecoder =
    D.list entryDecoder
    
entryDecoder : Decoder Entry
entryDecoder = 
    map6 Entry
        (field "uuid" string)
        (field "title" string)
        (field "author" string)
        (field "updated_on" string)
        (field "content" string)
        (field "created_on" string)
