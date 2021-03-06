module Pages.Play exposing (Model, Msg, page)

import Browser.Events
import Dict exposing (Dict)
import Effect exposing (Effect)
import Fifo exposing (Fifo)
import Gen.Params.Play exposing (Params)
import Html exposing (..)
import Html.Attributes as A exposing (..)
import Html.Events as E exposing (..)
import Json.Decode as D
import Model exposing (LogEntry, Point)
import Page
import Random
import Request
import Shared
import Time exposing (Posix)
import View exposing (View)


page : Shared.Model -> Request.With Params -> Page.With Model Msg
page shared req =
    Page.advanced
        { init = init
        , update = update req
        , view = View.shared view shared
        , subscriptions = subscriptions
        }



-- INIT


type alias Model =
    Maybe OkModel


type alias OkModel =
    { pts : Fifo Point
    , animations : List LogEntry
    , now : Posix
    , updated : Posix
    }


init : ( Model, Effect Msg )
init =
    ( Nothing, Effect.none )



-- UPDATE


type Msg
    = OnAnimationFrame Posix
    | OnNextPoint Point
    | OnInitPoints (List Point)
    | OnClick Point Point


update : Request.With Params -> Msg -> Model -> ( Model, Effect Msg )
update req msg model =
    case model of
        Nothing ->
            updateLoading req msg

        Just ok ->
            ok |> updateOk msg |> Tuple.mapFirst Just


updateLoading : Request.With Params -> Msg -> ( Model, Effect Msg )
updateLoading req msg =
    case msg of
        OnAnimationFrame now ->
            ( Just { pts = Fifo.empty, now = now, updated = now, animations = [] }
            , let
                n : Int
                n =
                    req.query
                        |> Dict.get "preview"
                        |> Maybe.andThen String.toInt
                        |> Maybe.withDefault 2
              in
              Model.genPoint |> Random.list (1 + Basics.max 0 n) |> Random.generate OnInitPoints |> Effect.fromCmd
            )

        _ ->
            ( Nothing, Effect.none )


updateOk : Msg -> OkModel -> ( OkModel, Effect Msg )
updateOk msg model =
    case msg of
        OnAnimationFrame now ->
            ( { model | now = now }, Effect.none )

        OnNextPoint pt ->
            ( { model | pts = model.pts |> Fifo.insert pt, updated = model.now }, Effect.none )

        OnInitPoints pts ->
            ( { model | pts = pts |> Fifo.fromList, updated = model.now }, Effect.none )

        OnClick pt dim ->
            case Fifo.remove model.pts of
                ( Nothing, _ ) ->
                    ( model, Effect.none )

                ( Just expected, pts ) ->
                    let
                        log : Model.LogEntry
                        log =
                            { expected = expected
                            , actual = pt
                            , displayed = model.updated
                            , clicked = model.now
                            , dimensions = Just dim
                            }
                    in
                    ( { model | pts = pts, animations = log :: List.filter (isAnimationVisible model.now) model.animations }
                    , Effect.batch
                        [ Effect.fromCmd <| Random.generate OnNextPoint Model.genPoint
                        , Effect.fromShared <|
                            Shared.OnLogEntry log
                        ]
                    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onAnimationFrame OnAnimationFrame



-- VIEW


view : Shared.OkModel -> Model -> View Msg
view shared model =
    { title = "Play"
    , body = model |> Maybe.map (viewOk shared) |> Maybe.withDefault []
    }


viewOk : Shared.OkModel -> OkModel -> List (Html Msg)
viewOk shared model =
    [ div [ class "play" ]
        [ viewHud shared model
        , viewInput
        ]
    ]


viewHud : Shared.OkModel -> OkModel -> Html msg
viewHud shared model =
    div [ class "play-hud" ]
        (pre [] [ text "Swipe back to pause\n", shared.logs |> Model.buildStatistics |> Model.renderStatistics |> String.join "\n" |> text ]
            :: (case model.pts |> Fifo.remove of
                    ( Just pt, previews ) ->
                        viewPt pt
                            :: viewPreviews (Fifo.toList previews)

                    ( Nothing, previews ) ->
                        div [] []
                            :: viewPreviews (Fifo.toList previews)
               )
            ++ viewAnimations model.now model.animations
        )


viewPt : Point -> Html msg
viewPt ( x, y ) =
    div [ class "play-pt", style "top" <| renderPercent y, style "left" <| renderPercent x ] []


viewAnimations : Posix -> List LogEntry -> List (Html msg)
viewAnimations now =
    List.filterMap (viewAnimation now)


animationCycleSecs : Float
animationCycleSecs =
    0.8


isAnimationVisible : Posix -> LogEntry -> Bool
isAnimationVisible now log =
    Model.elapsedSec now log <= animationCycleSecs


viewAnimation : Posix -> LogEntry -> Maybe (Html msg)
viewAnimation now log =
    let
        ( ex, ey ) =
            log.expected

        ( ax, ay ) =
            log.actual

        cycle : Float
        cycle =
            Model.elapsedSec now log / animationCycleSecs
    in
    if cycle > 1 then
        Nothing

    else
        [ div
            [ class "play-postview play-expected"
            , style "top" <| renderPercent ey
            , style "left" <| renderPercent ex
            ]
            []
        , div
            [ class "play-postview play-actual"
            , style "top" <| renderPercent ay
            , style "left" <| renderPercent ax
            ]
            []
        , pre
            [ class "play-score"
            , style "top" <| renderPercent <| ay - cycle * 0.2
            , style "left" <| renderPercent <| clamp 0 0.65 ax
            , style "right" <| renderPercent ax
            ]
            [ log |> Model.renderScore |> String.join "\n" |> text ]
        ]
            |> div [ style "opacity" <| renderPercent <| 0.7 * (1 - cycle) ]
            |> Just


viewPreviews : List Point -> List (Html msg)
viewPreviews pts =
    List.indexedMap (viewPreview (pts |> List.length)) pts


viewPreview : Int -> Int -> Point -> Html msg
viewPreview size index ( x, y ) =
    div
        [ class "play-preview"
        , style "top" <| renderPercent y
        , style "left" <| renderPercent x
        , style "opacity" (0.8 * toFloat (size - index) / toFloat size |> String.fromFloat)
        , style "transform" <|
            if index == 0 then
                "scale(1.5)"

            else
                "unset"
        ]
        []


renderPercent : Float -> String
renderPercent pct =
    String.fromFloat (pct * 100) ++ "%"


viewInput : Html Msg
viewInput =
    div [ class "play-input", onClickPoint OnClick ] []


onClickPoint : (Point -> Point -> msg) -> Html.Attribute msg
onClickPoint msg =
    D.map2 msg
        (decodePointFromClickEvent )
        (decodeDimensionsFromClickEvent )
        |> E.on "click"


decodePointFromClickEvent : D.Decoder Point
decodePointFromClickEvent =
    D.map2 Tuple.pair decodeXFromClickEvent decodeYFromClickEvent

decodeDimensionsFromClickEvent : D.Decoder Point
decodeDimensionsFromClickEvent =
    D.map2 Tuple.pair
        (D.at [ "target", "offsetWidth" ] D.float)
        (D.at [ "target", "offsetHeight" ] D.float)


decodePercent : { value : D.Decoder Int, start : D.Decoder Int, size : D.Decoder Int } -> D.Decoder Float
decodePercent fn =
    D.map3
        (\value start size ->
            toFloat (value - start) / toFloat (Basics.max 1 size)
        )
        fn.value
        fn.start
        fn.size


decodeXFromClickEvent : D.Decoder Float
decodeXFromClickEvent =
    decodePercent
        { value = D.field "clientX" D.int
        , start = D.at [ "target", "offsetLeft" ] D.int
        , size = D.at [ "target", "offsetWidth" ] D.int
        }


decodeYFromClickEvent : D.Decoder Float
decodeYFromClickEvent =
    decodePercent
        { value = D.field "clientY" D.int
        , start = D.at [ "target", "offsetTop" ] D.int
        , size = D.at [ "target", "offsetHeight" ] D.int
        }
