module Shared exposing
    ( Flags
    , Model(..)
    , Msg(..)
    , OkModel
    , init
    , subscriptions
    , update
    )

import Json.Decode as D
import Json.Encode as E
import Model
import Ports
import Request exposing (Request)


type alias Flags =
    D.Value


type Model
    = Loading
    | Failed String
    | Ready OkModel


type alias OkModel =
    { logs : List Model.LogEntry }


type Msg
    = OnLogEntry Model.LogEntry
    | OnReset
    | OnPersistPull D.Value


init : Request -> Flags -> ( Model, Cmd Msg )
init _ _ =
    ( Loading, Ports.persistRequest () )


empty : OkModel
empty =
    { logs = [] }


update : Request -> Msg -> Model -> ( Model, Cmd Msg )
update _ msg model =
    case model of
        Loading ->
            case msg of
                OnPersistPull json ->
                    case D.decodeValue decode json of
                        Err err ->
                            ( Failed <| D.errorToString err, Cmd.none )

                        Ok ok ->
                            ( Ready ok, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        Failed err ->
            ( Failed err, Cmd.none )

        Ready ok ->
            updateOk msg ok


updateOk : Msg -> OkModel -> ( Model, Cmd msg )
updateOk msg model =
    case msg of
        OnLogEntry entry ->
            let
                m =
                    { model | logs = entry :: model.logs }
            in
            ( m |> Ready, m |> encode |> Ports.persistPush )

        OnReset ->
            ( empty |> Ready, Ports.persistClear () )

        OnPersistPull json ->
            case D.decodeValue decode json of
                Err err ->
                    ( Failed <| D.errorToString err, Cmd.none )

                Ok ok ->
                    ( Ready ok, Cmd.none )


subscriptions : Request -> Model -> Sub Msg
subscriptions _ _ =
    Ports.persistPull OnPersistPull


encode : OkModel -> E.Value
encode model =
    E.object
        [ ( "logs", model.logs |> Model.encodeLogs )
        ]


decode : D.Decoder OkModel
decode =
    D.oneOf
        [ D.null empty
        , D.map OkModel
            (D.field "logs" Model.decodeLogs)
        ]
