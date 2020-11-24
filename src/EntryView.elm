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
import Urls
import Types exposing (Model)
import Url exposing (Url)

getTimeOrderedEntryList : Entries -> List Entry
getTimeOrderedEntryList entries =
    List.reverse <| Dict.values entries.entries

viewEntryCards : Model -> Html Msg
viewEntryCards model =
    let
        url = model.url
        entriesList = getTimeOrderedEntryList model.entries
    in
        Keyed.node "div" [class "col-md-8 mt-3 left"] (List.map (viewKeyedEntryCard url) entriesList)

viewKeyedEntryCard : Url -> Entry -> (String, Html Msg)
viewKeyedEntryCard url entry =
    (entry.id, lazy (viewEntryCard url) entry)

viewEntryCard : Url -> Entry -> Html Msg
viewEntryCard url entry =
    div [class "card mb-8"] [
        div [class "card-body"] [
            h2 [class "card-title"] [text entry.title],
            p [class "card-text text-muted h6"] [text <| String.concat [entry.author, " | ", (String.slice 0 10 entry.created_on)]],
            p [class "card-text"] [text <| String.slice 0 199 entry.content],
            a [href (getEntryHref url entry), class "btn btn-secondary"] [text "Read More"],
            a [href (getEntryEditHref url entry), class "btn btn-secondary"] [text "Edit"],
            button [Html.Events.onClick Types.ClickDeleteEntry, class "btn btn-secondary"] [text "Delete"]
        ]
    ]

viewEntriesSidebar : Model -> Html Msg
viewEntriesSidebar model = 
    let
        url = model.url
        entries = model.entries
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
                Keyed.node "div" [class "card-body"] (List.map (viewKeyedEntrySidebar url) entriesList)
            ]
        ]

viewKeyedEntrySidebar : Url -> Entry -> (String, Html Msg)
viewKeyedEntrySidebar url entry =
    (entry.id, lazy (viewEntrySidebar url) entry)

viewEntrySidebar : Url -> Entry -> Html Msg
viewEntrySidebar url entry = 
    div [][a [href (getEntryHref url entry)][text ((String.slice 0 10 entry.created_on) ++ " " ++ entry.title)]]


getEntryHref : Url -> Entry -> String
getEntryHref url entry = 
    (Urls.getBasePath url) ++ "/entry/" ++ entry.created_on ++ "/" ++ entry.id ++ "/"

getEntryEditHref : Url -> Entry -> String
getEntryEditHref url entry = 
    (Urls.getBasePath url) ++ "/entry/edit/" ++ entry.created_on ++ "/" ++ entry.id

viewSingleEntry : Url -> Entry -> Html Msg
viewSingleEntry url entry = 
    div [class "col-md-8 mt-3 left"] [
        div [class "card-body"] [
            h1 [class "card-title"] [text entry.title],
            p [class "text-muted"] [text (entry.author ++ " | " ++ (String.slice 0 10 entry.created_on))],
            p [class "card-text"] [text entry.content],
            a [href (getEntryEditHref url entry), class "btn btn-secondary"] [text "Edit"],
            button [Html.Events.onClick Types.ClickDeleteEntry, class "btn btn-secondary"] [text "Delete"]
        ]
    ]

viewEditableEntry : Url -> Entry -> Html Msg
viewEditableEntry url entry =
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
            a [href (getEntryHref url entry), class "btn btn-secondary"] [text "Cancel"]
        ]
    ]

viewCreateEntry : Model -> Html Msg
viewCreateEntry model =
        div [class "col-md-8 mt-3 left"] [
        div [class "card-body", id "form"] [
            input [
                class "card-title", 
                style "min-width" "50%", 
                placeholder "Title",
                Html.Events.onInput Types.UpdateEntryTitle
            ] [],
            div [class "form-group"][
                textarea [
                    style "min-width" "100%",
                    placeholder "Your journal entry",
                    Html.Events.onInput Types.UpdateEntryContent
                    ][]
                ],
            button [Html.Events.onClick Types.ClickCreateEntry, class "btn btn-secondary"] [text "Save"],
            a [href (Urls.getBasePath model.url), class "btn btn-secondary"] [text "Cancel"]
        ]
    ]
