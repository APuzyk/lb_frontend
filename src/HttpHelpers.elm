module HttpHelpers exposing (patch)

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