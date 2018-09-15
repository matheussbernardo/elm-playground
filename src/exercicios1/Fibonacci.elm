module Fibonacci exposing (init, main, update, view)

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
    { input : String, out : List Int }


init : Model
init =
    { input = "", out = [] }



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
        [ h1 [] [ viewList m.out ]
        , input [ placeholder "Type here", onInput Replace ] []
        , button [ onClick (inputClick m.input) ] [ text "Fibonacci?" ]
        ]


fibList : Int -> List Int -> List Int
fibList n acc =
    if n == 0 then
        acc
    else
        fibList (n-1) (fibNext acc)

    -- How fibList works     
    -- fibList 4-1  fibNext []    [1]
    -- fibList 3-1  fibNext [1] > [1,1] 
    -- fibList 2-1  fibNext [1,1] > [2,1,1]
    -- fibList 1-1  fibNext [2,1,1] > [3,2,1,1]
    -- fibList 0 acc > [3,2,1,1]


fibNext lst =
    case lst of
        [] ->
            [ 1 ]

        [ x ] ->
            [ 1, x ]

        x :: y :: tail ->
            (x + y) :: lst
            

fib : Int -> Int
fib n =
    if n <= 2 then
        1
    else
        fib (n - 1) + fib (n - 2)

inputFib : Maybe Int -> List Int
inputFib n =
    let v = Maybe.withDefault 0 n
    in
    fibList v [] |> List.reverse


viewList : List Int -> Html Msg
viewList lst =
    let
        makeLi n =
            li [] [ text (String.fromInt n) ]

        children =
            List.map makeLi lst
    in
    ol [] children


inputClick : String -> Msg
inputClick str =
    Send (str |> String.toInt)

