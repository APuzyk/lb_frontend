module EntryUpdate exposing (..)
import Http
import HttpHelpers as HttpH
import Json.Encode as Encode
import Types exposing (Model, Msg)
import Entry exposing (Entry, Entries)
import EntryPull as EP
import EntryView exposing (getEntryHref)
import Dict
import Auth exposing (httpErrorToString)
import Browser.Navigation as Nav
import Urls
updateEntry : Model -> Entry -> Cmd Msg
updateEntry model entry = 
    let
        entries = model.entries
        user = model.user

        body = 
            entry
                |> patchEntryEncoder
                |> Http.jsonBody
        
        apiUrl = Urls.api ++ "entry/" ++ entry.id ++ "/"
        
    in
        HttpH.patch 
            user.accessToken
            { url = apiUrl
            , body = body
            , expect= Http.expectJson Types.PatchedEntry EP.entryDecoder
            }

patchEntryEncoder : Entry -> Encode.Value
patchEntryEncoder entry =
    Encode.object
        [ ("title", Encode.string entry.title)
        , ("content", Encode.string entry.content)
        ]     

patchedEntryCompleted :  Model -> Result Http.Error Entry -> ( Model, Cmd Msg )
patchedEntryCompleted model result =
    case result of
        Ok entry ->
            let 
                entries = model.entries
                newEntries = {entries | entries = Dict.insert (EP.genEntryKey entry) entry entries.entries,
                                        activeEntry = Just entry,
                                        editable = False } 
            in
                ( { model | entries = newEntries, errorMsg = "" }, 
                Nav.pushUrl model.key  (getEntryHref entry))
        
        Err error ->
            ( { model | errorMsg = (httpErrorToString error) }, Cmd.none )