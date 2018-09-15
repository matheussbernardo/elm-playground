module TemporimFun exposing (init, main, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


main =
    Browser.sandbox
        { view = view
        , update = update
        , init = init
        }



-- MODEL --------------------------------------------------


type alias Model =
    { input : String, out : String }


init : Model
init =
    { input = "", out = "" }



-- MESSAGES ----------------------------------------------


type Msg
    = Concat String
    | Replace String


update : Msg -> Model -> Model
update msg m =
    case msg of
        Concat st ->
            { m | out = String.fromInt (quaodeboas (DeBoas 2)) }

        Replace st ->
            { m | input = st }

type Temporim = Estressado Int | DeBoas Int

quaodeboas : Temporim -> Int
quaodeboas t =
        case t of
            Estressado v -> 0
            DeBoas v -> v



-- VIEW --------------------------------------------------


view : Model -> Html Msg
view m =
    div []
        [ h1 [] [ text m.out ]
        , input [ placeholder "Type here", onInput Replace ] []
        , button [ onClick (Concat m.input) ] [ text "Hello?" ]
        ]
