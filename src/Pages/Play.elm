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
    | OnClick Point


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
            ( Just { pts = Fifo.empty, now = now, updated = now }
            , let
                n : Int
                n =
                    req.query
                        |> Dict.get "preview"
                        |> Maybe.andThen String.toInt
                        |> Maybe.withDefault 0
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

        OnClick pt ->
            case Fifo.remove model.pts of
                ( Nothing, _ ) ->
                    ( model, Effect.none )

                ( Just expected, pts ) ->
                    ( { model | pts = pts }
                    , Effect.batch
                        [ Effect.fromCmd <| Random.generate OnNextPoint Model.genPoint
                        , Effect.fromShared <|
                            Shared.OnLogEntry
                                { expected = expected
                                , actual = pt
                                , displayed = model.updated
                                , clicked = model.now
                                }
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
                        viewPt pt :: viewPreviews (Fifo.toList previews)

                    ( Nothing, previews ) ->
                        div [] [] :: List.indexedMap (viewPreview (previews |> Fifo.toList |> List.length)) (previews |> Fifo.toList)
               )
        )


viewPt : Point -> Html msg
viewPt ( x, y ) =
    div [ class "play-pt", style "top" <| renderPercent y, style "left" <| renderPercent x ] []


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
        ]
        []


renderPercent : Float -> String
renderPercent pct =
    String.fromFloat (pct * 100) ++ "%"


viewInput : Html Msg
viewInput =
    div [ class "play-input", onClickPoint OnClick ] []


onClickPoint : (Point -> msg) -> Html.Attribute msg
onClickPoint msg =
    E.on "click" (decodePointFromClickEvent |> D.map msg)


decodePointFromClickEvent : D.Decoder Point
decodePointFromClickEvent =
    D.map2 Tuple.pair decodeXFromClickEvent decodeYFromClickEvent


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
