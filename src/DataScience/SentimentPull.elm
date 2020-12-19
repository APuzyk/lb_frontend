module DataScience.SentimentPull exposing (..)

import Types as T exposing (Msg, Model)
import Http
import Jwt.Http
import Urls exposing (entriesUrl)
import Json.Decode as D exposing (..)
import Entry exposing (Entry, Entries, TmpEntryList)
import Auth exposing (httpErrorToString)
import Dict exposing (Dict)
import Urls exposing (sentimentUrl)
import DataScience.DataStructures exposing(SentimentScore)


    
getSentimentScoresCompleted :  Model -> Result Http.Error (List SentimentScore) -> ( Model, Cmd Msg )
getSentimentScoresCompleted model result =
    case result of
        Ok sentimentScores ->
            let 
                oldDatascience = model.datascience
                newDatascience = {oldDatascience | sentimentScores = sentimentScores}
            in
                ( 
                    { model | datascience = newDatascience, errorMsg = "" }, 
                    Cmd.none
                )
        
        Err error ->
            ( { model | errorMsg = (httpErrorToString error) }, Cmd.none )

getSentimentScores : Model -> Cmd Msg
getSentimentScores model =
    let
        user = model.user
    in
        Jwt.Http.get user.accessToken
            { url = sentimentUrl model.url
            , expect = Http.expectJson T.GotSentimentScores sentimentScoresDecoder}



sentimentScoresDecoder : Decoder (List SentimentScore)
sentimentScoresDecoder =
    field "results" sentimentScoreDecoder


sentimentScoreDecoder : Decoder (List SentimentScore)
sentimentScoreDecoder = 
    D.list <|
        map2 SentimentScore
            (field "entry_created_on" string)
            (field "sentiment" float)
