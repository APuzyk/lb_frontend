module EntryCreate exposing (..)

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
import Jwt.Http as JwtH
import EntryUpdate as EU
createEntry : Model -> Entry -> Cmd Msg
createEntry model entry = 
    let
        entries = model.entries
        user = model.user

        body = 
            entry
                |> EU.patchEntryEncoder
                |> Http.jsonBody
        
        apiUrl = (Urls.api model.url) ++ "entry/"
        
    in
            JwtH.post
            user.accessToken
            { url = apiUrl
            , body = body
            , expect= Http.expectJson Types.CreatedEntry EP.entryDecoder
            }
