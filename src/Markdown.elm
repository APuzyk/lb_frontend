module Markdown exposing (..)
import Html exposing (Html, text)
import Html.Attributes as Attr
import Html.Events
import Markdown.Parser as Markdown
import Markdown.Renderer


parseMarkdownString : String -> List(Html msg)
parseMarkdownString markdown = 
    case
        markdown
            |> Markdown.parse
            |> Result.mapError deadEndsToString
            |> Result.andThen (\ast -> Markdown.Renderer.render Markdown.Renderer.defaultHtmlRenderer ast)
    of
        Ok rendered ->
            rendered
        Err errors ->
            [text errors]

deadEndsToString deadEnds =
    deadEnds
        |> List.map Markdown.deadEndToString
        |> String.join "\n"