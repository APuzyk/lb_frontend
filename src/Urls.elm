module Urls exposing (..)
import Url.Parser exposing (Parser, (</>), int, map, oneOf, s, string)

api : String
api =
    "http://localhost:8080/"
registerUrl : String
registerUrl =
    let
        apiUrl = api
    in
        apiUrl ++ "api/token/"

entriesUrl : String
entriesUrl =
    let
        apiUrl = api
    in
        apiUrl ++ "entry/"


