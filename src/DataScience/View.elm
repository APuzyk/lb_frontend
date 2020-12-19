module DataScience.View exposing (..)

import Browser
import Types exposing (Model, Msg)
import Html exposing (div, h5, text)
import Html.Attributes exposing (..)
import Base as B
import Visualization.SentimentLine exposing (sentimentLine)

viewInsights : Model -> Browser.Document Msg
viewInsights model = 
    let
        datascience = model.datascience
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
                                            div [class "card-body"] [sentimentLine datascience.sentimentScores]
                                            ]
                                        ]
                                ]   
                                ]
                            , B.viewFooter
                        ]
                }
