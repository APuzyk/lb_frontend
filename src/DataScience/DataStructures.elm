module DataScience.DataStructures exposing (..)

type alias Datascience =
  { sentimentScores: (List SentimentScore)
  }

type alias SentimentScore =
    { entry_created_on : String
    , score : Float
    }