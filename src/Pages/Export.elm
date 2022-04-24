module Pages.Export exposing (page)

import Gen.Params.Export exposing (Params)
import Html exposing (..)
import Html.Attributes exposing (..)
import Json.Encode as E
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
    { title = "Export"
    , body =
        [ div [ class "main export" ]
            [ h1 [] [ text "Export" ]
            , p [] [ text "copy and paste into some other app. maybe someday I'll make a better way to export" ]
            , textarea [ readonly True ] [ text <| E.encode 2 <| Model.encodeLogs shared.logs ]
            ]
        ]
    }
