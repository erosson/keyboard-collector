module Pages.Home_ exposing (page)

import Gen.Params.Home_ exposing (Params)
import Gen.Route
import Html exposing (..)
import Html.Attributes exposing (..)
import Model
import Page exposing (Page)
import Request
import Shared
import View exposing (View)


page : Shared.Model -> Request.With Params -> Page
page shared req =
    Page.static
        { view = View.shared view shared ()
        }


view : Shared.OkModel -> () -> View msg
view shared _ =
    { title = "Keyboard data collector"
    , body =
        [ div [ class "main" ]
            [ h1 [] [ text "Keyboard data collector" ]
            , pre [] [ shared.logs |> Model.buildStatistics |> Model.renderStatistics |> String.join "\n" |> text ]
            , p [] [ a [ href <| Gen.Route.toHref Gen.Route.Play ] [ text "Start" ] ]
            , p [] [ a [ href <| Gen.Route.toHref Gen.Route.Play ++ "?preview=2" ] [ text "Start (with previews)" ] ]
            , p [] [ a [ href <| Gen.Route.toHref Gen.Route.Export ] [ text "Export" ] ]
            , p [] [ a [ href <| Gen.Route.toHref Gen.Route.Reset ] [ text "Reset" ] ]
            ]
        ]
    }
