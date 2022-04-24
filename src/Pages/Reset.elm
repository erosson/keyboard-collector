module Pages.Reset exposing (Model, Msg, page)

import Effect exposing (Effect)
import Gen.Params.Reset exposing (Params)
import Gen.Route
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Page
import Request
import Shared
import View exposing (View)


page : Shared.Model -> Request.With Params -> Page.With Model Msg
page shared req =
    Page.advanced
        { init = init
        , update = update req
        , view = view
        , subscriptions = subscriptions
        }



-- INIT


type alias Model =
    {}


init : ( Model, Effect Msg )
init =
    ( {}, Effect.none )



-- UPDATE


type Msg
    = OnReset


update : Request.With Params -> Msg -> Model -> ( Model, Effect Msg )
update req msg model =
    case msg of
        OnReset ->
            ( model
            , Effect.batch
                [ Effect.fromShared Shared.OnReset
                , Effect.fromCmd <| Request.pushRoute Gen.Route.Home_ req
                ]
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> View Msg
view model =
    { title = "Reset"
    , body =
        [ div [ class "main" ]
            [ h1 [] [ text "Reset" ]
            , p [] [ text "Are you sure you want to reset? There is no undo." ]
            , button [ onClick OnReset ] [ text "Reset" ]
            ]
        ]
    }
