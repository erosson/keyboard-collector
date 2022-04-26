module Model exposing (..)

import Json.Decode as D
import Json.Encode as E
import Random
import Time exposing (Posix)


type alias Point =
    ( Float, Float )


type alias LogEntry =
    { expected : Point
    , actual : Point
    , displayed : Posix
    , clicked : Posix
    -- hud size; top half of the screen. `Maybe` for compatibility with old saved data
    , dimensions: Maybe Point
    }


genPoint : Random.Generator Point
genPoint =
    let
        gen01 =
            Random.float 0 1
    in
    Random.map2 Tuple.pair gen01 gen01


type alias Statistics =
    { count : Int
    , latency : Float
    , xyError : Float
    , xySquaredError : Float
    }


buildStatistics : List LogEntry -> Statistics
buildStatistics logs =
    { count = logs |> List.length
    , latency = logs |> List.map (toLatency >> toFloat) |> mean
    , xyError = logs |> List.map toXYError |> mean
    , xySquaredError = logs |> List.map (toXYError >> (\d -> d * d)) |> mean
    }


mean : List Float -> Float
mean ns =
    (ns |> List.sum) / (ns |> List.length |> toFloat)


toLatency : LogEntry -> Int
toLatency log =
    Time.posixToMillis log.clicked - Time.posixToMillis log.displayed


toXYError : LogEntry -> Float
toXYError log =
    let
        ( ex, ey ) =
            log.expected

        ( ax, ay ) =
            log.actual

        dx =
            ax - ex

        dy =
            ay - ey
    in
    sqrt (dx * dx + dy * dy)


renderStatistics : Statistics -> List String
renderStatistics stats =
    if stats.count == 0 then
        [ "Entry count: 0" ]

    else
        [ "Entry count: " ++ String.fromInt stats.count
        , "Latency: " ++ String.fromFloat stats.latency
        , "XY error: " ++ String.fromFloat stats.xyError
        , "XY error^2: " ++ String.fromFloat stats.xySquaredError
        ]


renderScore : LogEntry -> List String
renderScore =
    List.singleton >> buildStatistics >> renderStatistics >> List.drop 1


elapsedSec : Posix -> LogEntry -> Float
elapsedSec now log =
    toFloat (Time.posixToMillis now - Time.posixToMillis log.clicked) / 1000



-- encoders/decoders


encodeLogs : List LogEntry -> E.Value
encodeLogs =
    E.list encodeLog


encodeLog : LogEntry -> E.Value
encodeLog log =
    E.object
        ([ ( "expected", encodePoint log.expected )
        , ( "actual", encodePoint log.actual )
        , ( "displayed", encodePosix log.displayed )
        , ( "clicked", encodePosix log.clicked )
        ] ++ (case log.dimensions of
            Nothing -> []
            Just dim -> [("dimensions", encodePoint dim)]
        ))


encodePoint : Point -> E.Value
encodePoint ( x, y ) =
    E.list identity [ E.float x, E.float y ]


encodePosix : Posix -> E.Value
encodePosix =
    Time.posixToMillis >> E.int


decodeLogs : D.Decoder (List LogEntry)
decodeLogs =
    D.list decodeLog


decodeLog : D.Decoder LogEntry
decodeLog =
    D.map5 LogEntry
        (D.field "expected" decodePoint)
        (D.field "actual" decodePoint)
        (D.field "displayed" decodePosix)
        (D.field "clicked" decodePosix)
        (D.maybe <| D.field "dimensions" decodePoint)


decodePoint : D.Decoder Point
decodePoint =
    D.map2 Tuple.pair (D.index 0 D.float) (D.index 1 D.float)


decodePosix : D.Decoder Posix
decodePosix =
    D.map Time.millisToPosix D.int
