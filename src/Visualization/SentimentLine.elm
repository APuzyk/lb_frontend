module Visualization.SentimentLine exposing (..)
import Axis
import Color exposing (Color)
import Path exposing (Path)
import Scale exposing (ContinuousScale)
import Shape
import Time
import TypedSvg exposing (g, svg)
import TypedSvg.Attributes exposing (class, fill, stroke, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (strokeWidth)
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (Paint(..), Transform(..))
import Iso8601 exposing (toTime)
import DataScience.DataStructures exposing (SentimentScore)



w : Float
w =
    900


h : Float
h =
    450


padding : Float
padding =
    30


xScale : List (Time.Posix) -> ContinuousScale Time.Posix
xScale times =
    let
        maybeMin = List.minimum (List.map Time.posixToMillis times)
        maybeMax = List.maximum (List.map Time.posixToMillis times)
    in
    case (maybeMin, maybeMax) of 
        (Just min, Just max) ->
            Scale.time Time.utc ( 0, w - 2 * padding ) ( Time.millisToPosix min, Time.millisToPosix max )
        _ ->
            Scale.time Time.utc ( 0, w - 2 * padding ) (Time.millisToPosix 0, Time.millisToPosix 0)


yScale : ContinuousScale Float
yScale =
    Scale.linear ( h - 2 * padding, 0 ) ( 0, 1 )


xAxis : List ( Time.Posix, Float ) -> Svg msg
xAxis model =
    Axis.bottom [ Axis.tickCount (List.length model) ] (xScale (List.map Tuple.first model))


yAxis : Svg msg
yAxis =
    Axis.left [ Axis.tickCount 5 ] yScale


transformToLineData :  ContinuousScale Time.Posix -> ( Time.Posix, Float ) -> Maybe ( Float, Float )
transformToLineData thisXScale ( x, y ) =
    Just ( Scale.convert thisXScale x, Scale.convert yScale y )


tranfromToAreaData :  ContinuousScale Time.Posix -> ( Time.Posix, Float ) -> Maybe ( ( Float, Float ), ( Float, Float ) )
tranfromToAreaData thisXScale ( x, y ) =
    Just
        ( ( Scale.convert thisXScale x, Tuple.first (Scale.rangeExtent yScale) )
        , ( Scale.convert thisXScale x, Scale.convert yScale y )
        )


line : List ( Time.Posix, Float ) -> Path
line model =
    List.map (transformToLineData (xScale (List.map Tuple.first model))) model
        |> Shape.line Shape.monotoneInXCurve


area : List ( Time.Posix, Float ) -> Path
area model =
    List.map (tranfromToAreaData (xScale (List.map Tuple.first model))) model
        |> Shape.area Shape.monotoneInXCurve


sentimentLine : List (SentimentScore) -> Svg msg
sentimentLine sentimentScores =
    let
        model = scoreMapper sentimentScores
    in
    
    svg [ viewBox 0 0 w h ]
        [ g [ transform [ Translate (padding - 1) (h - padding) ] ]
            [ xAxis model ]
        , g [ transform [ Translate (padding - 1) padding ] ]
            [ yAxis ]
        , g [ transform [ Translate padding padding ], class [ "series" ] ]
            [ Path.element (area model) [ strokeWidth 3, fill <| Paint <| Color.rgba 1 0 0 0.54 ]
            , Path.element (line model) [ stroke <| Paint <| Color.rgb 1 0 0, strokeWidth 3, fill PaintNone ]
            ]
        ]

scoreMapper : List (SentimentScore) -> List (Time.Posix, Float)
scoreMapper scores = 
    List.map (\score -> ((timeWithZeroOnFailure score.entry_created_on), score.score)) scores

timeWithZeroOnFailure : String -> Time.Posix
timeWithZeroOnFailure time =
    case toTime time of
        Ok ptime ->
            ptime
        Err error ->
            Time.millisToPosix 0
