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
    = Send (Maybe Int)
    | Replace String


update : Msg -> Model -> Model
update msg m =
    case msg of
        Send number ->
            { m | out = inputFib number }

        Replace st ->
            { m | input = st }



-- VIEW --------------------------------------------------


view : Model -> Html Msg
view m =
    div []
        [ h1 [] [ text m.out ]
        , input [ placeholder "Type here", onInput Replace ] []
        , button [ onClick (inputClick m.input) ] [ text "Fibonacci?" ]
        ]


fib : Int -> Int
fib n =
    if n <= 2 then
        1
    else
        fib (n - 1) + fib (n - 2)

inputFib : Maybe Int -> String
inputFib n =
    Maybe.withDefault 0 n
        |> fib
        |> String.fromInt


inputClick : String -> Msg
inputClick str =
    Send (str |> String.toInt)

