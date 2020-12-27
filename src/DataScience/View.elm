module DataScience.View exposing (..)

import Browser
import Types exposing (Model, Msg)
import Html exposing (div, h5, text)
import Html.Attributes exposing (..)
import Html.Lazy exposing (lazy)
import Base as B
import Visualization.SentimentLine exposing (sentimentLine)
import DataScience.WordCloud exposing (viewCloud)

viewInsights : Model -> Browser.Document Msg
viewInsights model = 
    let
        datascience = model.datascience
        entries = model.entries
    in
        { title = "Leatherbound"
                        , body =
                            [ B.viewNavbar model
                            , div [ class "container-fluid" ]
                                [
                                    div [class "row"] [
                                        div [class "col-md-6"][
                                            div [class "card my-6"][
                                                h5 [class "card-header"] [text "Sentiment"],
                                            div [class "card-body"] [lazy sentimentLine datascience.sentimentScores]
                                            ]
                                        ],
                                        div [class "col-md-6"][
                                            div [class "card my-6"][
                                                h5 [class "card-header"] [text "Word Cloud"],
                                            div [class "card-body"] (viewCloud entries.entries)
                                            ]
                                        ]
                                ]   
                                ]
                            , B.viewFooter
                        ]
                }
