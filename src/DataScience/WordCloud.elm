module DataScience.WordCloud exposing (..)

import Regex
import Json.Decode exposing (string)
import Dict exposing (Dict)
import Entry exposing(Entry)
import Html exposing (Html)
import Html exposing (li, text)
--import Htlm.Lazy exposing (lazy)

viewCloud : Dict String Entry -> List(Html msg)
viewCloud entryList =
    let
        topFive = wordCount entryList
    in
        List.map (\l -> li [] [ text (Tuple.first l ++ ": " ++ (String.fromInt <| Tuple.second l)) ]) topFive



wordCount : Dict String Entry -> List((String, Int))
wordCount entryList =
    List.take 10
        <| List.sortWith wordListSort
        <| Dict.toList
        <| List.foldr addWord Dict.empty
        <| List.filter notInStopWords 
        <| List.concatMap (String.split " ")
        <| List.map String.toLower 
        <| List.map trimWhitespace
        <| List.map removePunctuation
        <| List.map removeTags
        <| List.map getContent 
        <| Dict.values entryList

getContent : Entry -> String
getContent entry =
    entry.content

removeTags : String -> String
removeTags string =
    case Regex.fromString "<.*?>" of
        Nothing ->
            string

        Just regex ->
            Regex.replace regex (\_ -> "") string

removePunctuation : String -> String
removePunctuation string =
    case Regex.fromString "[^\\w\\s]|_/g" of
        Nothing ->
            string

        Just regex ->
            Regex.replace regex (\_ -> "") string

trimWhitespace : String -> String
trimWhitespace string = 
    case Regex.fromString "/\\s+/g" of 
        Nothing ->
            string
        Just regex ->
            Regex.replace regex (\_ -> " ") string

addWord : String -> Dict String Int -> Dict String Int
addWord word dict = 
    case Dict.get word dict of
        Just value ->
            Dict.insert word (value + 1) dict
        Nothing ->
            Dict.insert word 1 dict
notInStopWords : String -> Bool
notInStopWords word =
    not <| Dict.member word stopWords

wordListSort : (a, comparable) -> (b, comparable) -> Order
wordListSort a b =
    case compare (Tuple.second a) (Tuple.second b) of
      LT -> GT
      EQ -> EQ
      GT -> LT
    
stopWords : Dict String Int
stopWords = 
    Dict.fromList 
        [ ("", 1)
        , ("i",  1)
        , ("me",  1)
        , ("my",  1)
        , ("myself",  1)
        , ("we",  1)
        , ("our",  1)
        , ("ours",  1)
        , ("ourselves",  1)
        , ("you",  1)
        , ("your",  1)
        , ("yours",  1)
        , ("yourself",  1)
        , ("yourselves",  1)
        , ("he",  1)
        , ("him",  1)
        , ("his",  1)
        , ("himself",  1)
        , ("she",  1)
        , ("her",  1)
        , ("hers",  1)
        , ("herself",  1)
        , ("it",  1)
        , ("its",  1)
        , ("itself",  1)
        , ("they",  1)
        , ("them",  1)
        , ("their",  1)
        , ("theirs",  1)
        , ("themselves",  1)
        , ("what",  1)
        , ("which",  1)
        , ("who",  1)
        , ("whom",  1)
        , ("this",  1)
        , ("that",  1)
        , ("these",  1)
        , ("those",  1)
        , ("am",  1)
        , ("is",  1)
        , ("are",  1)
        , ("was",  1)
        , ("were",  1)
        , ("be",  1)
        , ("been",  1)
        , ("being",  1)
        , ("have",  1)
        , ("has",  1)
        , ("had",  1)
        , ("having",  1)
        , ("do",  1)
        , ("does",  1)
        , ("did",  1)
        , ("doing",  1)
        , ("a",  1)
        , ("an",  1)
        , ("the",  1)
        , ("and",  1)
        , ("but",  1)
        , ("if",  1)
        , ("or",  1)
        , ("because",  1)
        , ("as",  1)
        , ("until",  1)
        , ("while",  1)
        , ("of",  1)
        , ("at",  1)
        , ("by",  1)
        , ("for",  1)
        , ("with",  1)
        , ("about",  1)
        , ("against",  1)
        , ("between",  1)
        , ("into",  1)
        , ("through",  1)
        , ("during",  1)
        , ("before",  1)
        , ("after",  1)
        , ("above",  1)
        , ("below",  1)
        , ("to",  1)
        , ("from",  1)
        , ("up",  1)
        , ("down",  1)
        , ("in",  1)
        , ("out",  1)
        , ("on",  1)
        , ("off",  1)
        , ("over",  1)
        , ("under",  1)
        , ("again",  1)
        , ("further",  1)
        , ("then",  1)
        , ("once",  1)
        , ("here",  1)
        , ("there",  1)
        , ("when",  1)
        , ("where",  1)
        , ("why",  1)
        , ("how",  1)
        , ("all",  1)
        , ("any",  1)
        , ("both",  1)
        , ("each",  1)
        , ("few",  1)
        , ("more",  1)
        , ("most",  1)
        , ("other",  1)
        , ("some",  1)
        , ("such",  1)
        , ("no",  1)
        , ("nor",  1)
        , ("not",  1)
        , ("only",  1)
        , ("own",  1)
        , ("same",  1)
        , ("so",  1)
        , ("than",  1)
        , ("too",  1)
        , ("very",  1)
        , ("s",  1)
        , ("t",  1)
        , ("can",  1)
        , ("will",  1)
        , ("just",  1)
        , ("don",  1)
        , ("should",  1)
        , ("now", 1)
        ]