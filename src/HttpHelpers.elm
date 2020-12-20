module HttpHelpers exposing (patch, httpErrorToString)

import Http exposing (Expect, expectJson, header, jsonBody, request)
import Json.Decode as Decode exposing (Decoder, Value, field)


{-| Create a `PATCH` command with a token attached to the headers.
-}
patch : String -> { url : String, body : Http.Body, expect : Http.Expect msg } -> Cmd msg
patch =
    createRequest "PATCH"


createRequest : String -> String -> { url : String, body : Http.Body, expect : Http.Expect msg } -> Cmd msg
createRequest method token { url, body, expect } =
    let
        options =
            { method = method
            , headers = [ header "Authorization" ("Bearer " ++ token) ]
            , url = url
            , body = body
            , expect = expect
            , timeout = Nothing
            , tracker = Nothing
            }
    in
        request options

httpErrorToString : Http.Error -> String
httpErrorToString error =
    case error of
        Http.BadUrl url ->
            "The URL " ++ url ++ " was invalid"
        Http.Timeout ->
            "Unable to reach the server, try again"
        Http.NetworkError ->
            "Unable to reach the server, check your network connection"
        Http.BadStatus 500 ->
            "The server had a problem, try again later"
        Http.BadStatus 400 ->
            "Verify your information and try again"
        Http.BadStatus 401 ->
            "Incorrect Username or Password"
        Http.BadStatus _ ->
            "Unknown error"
        Http.BadBody errorMessage ->
            errorMessage

