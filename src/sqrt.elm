module App exposing (init, main, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Round exposing (round)


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
    = Concat (Maybe Float)
    | Replace String


update : Msg -> Model -> Model
update msg m =
    case msg of
        Concat number ->
            { m | out = inputSqrt number }

        Replace st ->
            { m | input = st }



-- VIEW --------------------------------------------------


view : Model -> Html Msg
view m =
    div []
        [ h1 [] [ text m.out ]
        , input [ placeholder "Type here", onInput Replace ] []
        , button [ onClick (inputClick m.input) ] [ text "Hello?" ]
        ]


sqrtAcc : Float -> Float -> Float -> Float
sqrtAcc x r acc =
    if round 3 acc == round 3 r then
        r
    else
        sqrtAcc x (((r ^ 2) + x)/ (2 * r)) r


sqrt : Float -> Float
sqrt x =
    sqrtAcc x (x / 2) 0


inputSqrt : Maybe Float -> String
inputSqrt n =
    Maybe.withDefault 0 n
        |> sqrt
        |> round 3


inputClick : String -> Msg
inputClick str =
    Concat (str |> String.toFloat)
