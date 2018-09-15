module App exposing (init, main, update, view)

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
    = Concat (Maybe Int)
    | Replace String


update : Msg -> Model -> Model
update msg m =
    case msg of
        Concat number ->
            { m | out = inputFat number }

        Replace st ->
            { m | input = st }



-- VIEW --------------------------------------------------


view : Model -> Html Msg
view m =
    div []
        [ h1 [] [ text m.out ]
        , input [ placeholder "Type here", onInput Replace ] []
        , button [ onClick (inputClick m.input) ] [ text "Factorial?" ]
        ]


fat : Int -> Int
fat n =
    if n <= 1 then
        1

    else
        n * fat (n - 1)


inputFat : Maybe Int -> String
inputFat n =
    Maybe.withDefault 0 n
        |> fat
        |> String.fromInt


inputClick : String -> Msg
inputClick str =
    Concat (str |> String.toInt)

