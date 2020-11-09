module EntryView exposing (..)

import Jwt.Http 
import Html.Attributes exposing (..)
import Http
import Html.Keyed as Keyed
import Html.Lazy exposing (lazy)
import Html exposing (..)
import String
import List
import Types exposing (Msg)
import Entry exposing (Entries, Entry)

viewEntryCards : Entries -> Html Msg
viewEntryCards entries =
    let
        entriesList = entries.entries
    in
        Keyed.node "div" [class "col-md-8 mt-3 left"] (List.map viewKeyedEntryCard entriesList)

viewKeyedEntryCard : Entry -> (String, Html Msg)
viewKeyedEntryCard entry =
    (entry.id, lazy viewEntryCard entry)

viewEntryCard : Entry -> Html Msg
viewEntryCard entry =
    div [class "card mb-4"] [
        div [class "card-body"] [
            h2 [class "card-title"] [text entry.title],
            p [class "card-text text-muted h6"] [text <| String.concat [entry.author, " | ", entry.created_on]],
            p [class "card-text"] [text <| String.slice 0 199 entry.content],
            a [href ("entry/" ++ entry.id), class "btn btn-secondary"] [text "Read More"],
            a [href ("entry/edit/" ++ entry.id), class "btn btn-secondary"] [text "Edit"],
            a [href ("entry/delete/" ++ entry.id), class "btn btn-secondary"] [text "Delete"]
        ]
    ]