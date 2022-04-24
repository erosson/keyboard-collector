module View exposing (View, map, none, placeholder, shared, toBrowserDocument)

import Browser
import Html exposing (..)
import Shared


type alias View msg =
    { title : String
    , body : List (Html msg)
    }


placeholder : String -> View msg
placeholder str =
    { title = str
    , body = [ Html.text str ]
    }


none : View msg
none =
    placeholder ""


map : (a -> b) -> View a -> View b
map fn view =
    { title = view.title
    , body = List.map (Html.map fn) view.body
    }


toBrowserDocument : View msg -> Browser.Document msg
toBrowserDocument view =
    { title = view.title
    , body = view.body
    }


shared : (Shared.OkModel -> model -> View msg) -> Shared.Model -> model -> View msg
shared viewNext shared_ model =
    case shared_ of
        Shared.Loading ->
            { title = "Loading...", body = [ pre [] [ text "Loading..." ] ] }

        Shared.Failed err ->
            { title = "Failed", body = [ pre [] [ text err ] ] }

        Shared.Ready ok ->
            viewNext ok model
