module EntryView exposing (..)

import Jwt.Http 
import Html.Attributes exposing (..)
import Http
import Html.Keyed as Keyed
import Html.Lazy exposing (lazy)
import Html.Events exposing(onClick)
import Html exposing (..)
import String
import List
import Types exposing (Msg)
import Entry exposing (Entries, Entry)
import Dict
import EntryPull exposing (entryDecoder)
import Urls exposing (basePath)

getTimeOrderedEntryList : Entries -> List Entry
getTimeOrderedEntryList entries =
    List.reverse <| Dict.values entries.entries

viewEntryCards : Entries -> Html Msg
viewEntryCards entries =
    let
        entriesList = getTimeOrderedEntryList entries
    in
        Keyed.node "div" [class "col-md-8 mt-3 left"] (List.map viewKeyedEntryCard entriesList)

viewKeyedEntryCard : Entry -> (String, Html Msg)
viewKeyedEntryCard entry =
    (entry.id, lazy viewEntryCard entry)

viewEntryCard : Entry -> Html Msg
viewEntryCard entry =
    div [class "card mb-8"] [
        div [class "card-body"] [
            h2 [class "card-title"] [text entry.title],
            p [class "card-text text-muted h6"] [text <| String.concat [entry.author, " | ", (String.slice 0 10 entry.created_on)]],
            p [class "card-text"] [text <| String.slice 0 199 entry.content],
            a [href (getEntryHref entry), class "btn btn-secondary"] [text "Read More"],
            a [href (getEntryEditHref entry), class "btn btn-secondary"] [text "Edit"],
            button [Html.Events.onClick Types.ClickDeleteEntry, class "btn btn-secondary"] [text "Delete"]
        ]
    ]

viewEntriesSidebar : Entries -> Html Msg
viewEntriesSidebar entries = 
    let
        entriesList = getTimeOrderedEntryList entries
        hide : String
        hide =
            if Dict.isEmpty entries.entries then
                " d-none"
            else
                ""
    in
        div [class ("col-md-3" ++ hide)] [
            div [class "card my-3"] [
                h5 [class "card-header"] [text "Entries"],
                Keyed.node "div" [class "card-body"] (List.map viewKeyedEntrySidebar entriesList)
            ]
        ]

viewKeyedEntrySidebar : Entry -> (String, Html Msg)
viewKeyedEntrySidebar entry =
    (entry.id, lazy viewEntrySidebar entry)

viewEntrySidebar : Entry -> Html Msg
viewEntrySidebar entry = 
    div [][a [href (getEntryHref entry)][text ((String.slice 0 10 entry.created_on) ++ " " ++ entry.title)]]


getEntryHref : Entry -> String
getEntryHref entry = 
    basePath ++ "/entry/" ++ entry.created_on ++ "/" ++ entry.id ++ "/"

getEntryEditHref : Entry -> String
getEntryEditHref entry = 
    basePath ++ "/entry/edit/" ++ entry.created_on ++ "/" ++ entry.id

viewSingleEntry : Entry -> Html Msg
viewSingleEntry entry = 
    div [class "col-md-8 mt-3 left"] [
        div [class "card-body"] [
            h1 [class "card-title"] [text entry.title],
            p [class "text-muted"] [text (entry.author ++ " | " ++ (String.slice 0 10 entry.created_on))],
            p [class "card-text"] [text entry.content],
            a [href (getEntryEditHref entry), class "btn btn-secondary"] [text "Edit"],
            button [Html.Events.onClick Types.ClickDeleteEntry, class "btn btn-secondary"] [text "Delete"]
        ]
    ]

viewEditableEntry : Entry -> Html Msg
viewEditableEntry entry =
        div [class "col-md-8 mt-3 left"] [
        div [class "card-body", id "form"] [
            input [
                class "card-title", 
                style "min-width" "50%", 
                value entry.title,
                Html.Events.onInput Types.UpdateEntryTitle
            ] [],
            p [class "text-muted"] [text (entry.author ++ " | " ++ (String.slice 0 10 entry.created_on))],
            div [class "form-group"][
                textarea [
                    style "min-width" "100%",
                    value entry.content,
                    Html.Events.onInput Types.UpdateEntryContent
                    ][]
                ],
            button [Html.Events.onClick Types.ClickSaveEntry, class "btn btn-secondary"] [text "Save"],
            a [href (getEntryHref entry), class "btn btn-secondary"] [text "Cancel"]
        ]
    ]

