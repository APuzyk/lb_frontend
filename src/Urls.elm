module Urls exposing (..)
import Url.Parser exposing (Parser, (</>), int, map, oneOf, s, string)
import Url exposing(Protocol, Url)
import Url exposing (Protocol(..))
import String exposing (fromInt)

getBasePath : Url -> String
getBasePath url = 
    let
        protocol = protocolToString url.protocol
    in
        case url.port_ of
            Just portNum ->
                protocol ++ url.host ++ ":" ++ (fromInt portNum)
            Nothing ->
                protocol ++ url.host 

-- API URLS
api : Url -> String
api url =
    (getBasePath url) ++ "/api/"
            

registerUrl : Url -> String
registerUrl url =
    let
        apiUrl = api url
    in
        apiUrl ++ "token/"

createUserUrl : Url -> String
createUserUrl url =
    let
        apiUrl = api url
    in
        apiUrl ++ "register/"
entriesUrl : Url -> String
entriesUrl url =
    let
        apiUrl = api url
    in
        apiUrl ++ "entry/"

sentimentUrl : Url -> String
sentimentUrl url =
    let
        apiUrl = api url
    in
        apiUrl ++ "datascience/sentiment/"


-- WEB APP URLS
createEntryUrl : Url -> String
createEntryUrl url =
    let
        basePath = getBasePath url
    in
        basePath ++ "/write_entry"

insightsUrl : Url -> String
insightsUrl url =
    let
        basePath = getBasePath url
    in
        basePath ++ "/insights"


protocolToString : Protocol -> String
protocolToString protocol =
        case protocol of 
        Https ->
            "https://"
        Http ->
            "http://"

getRegisterPath : Url -> String
getRegisterPath url = 
    (getBasePath url) ++ "/register/"

