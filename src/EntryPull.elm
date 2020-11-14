module EntryPull exposing (..)

import Types as T exposing (Msg, Model)
import Http
import Jwt.Http
import Urls exposing (entriesUrl)
import Json.Decode as D exposing (..)
import Entry exposing (Entry, Entries, TmpEntryList)
import Auth exposing (httpErrorToString)
import Dict exposing (Dict)


    
getEntriesCompleted :  Model -> Result Http.Error TmpEntryList -> ( Model, Cmd Msg )
getEntriesCompleted model result =
    case result of
        Ok entries ->
            let 
                oldEntries = model.entries
                newEntries = {oldEntries 
                                | next_page = entries.next_page, 
                                  entries = addEntries oldEntries.entries entries.entries
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


entriesDecoder : Decoder TmpEntryList
entriesDecoder = 
    map2 TmpEntryList
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

addEntries : Dict String Entry -> List Entry -> Dict String Entry
addEntries oldEntryDict newEntries = 
    Dict.union (genEntriesDict newEntries)  oldEntryDict

genEntriesDict : List Entry -> Dict String Entry
genEntriesDict entries = 
    Dict.fromList (List.map genEntryKV entries)

genEntryKV : Entry -> (String, Entry)
genEntryKV entry = 
    (entry.created_on ++ "_" ++ entry.id, entry)
