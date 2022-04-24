port module Ports exposing (..)

import Json.Encode as Json


port persistPush : Json.Value -> Cmd msg


port persistPull : (Json.Value -> msg) -> Sub msg


port persistRequest : () -> Cmd msg


port persistClear : () -> Cmd msg
