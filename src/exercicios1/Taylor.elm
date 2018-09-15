module Taylor exposing (init, main, update, view)

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
    = Send (Maybe Float)
    | Replace String


update : Msg -> Model -> Model
update msg m =
    case msg of
        Send number ->
            { m | out = inputExp number }

        Replace st ->
            { m | input = st }


inputExp : Maybe Float -> String
inputExp n =
    Maybe.withDefault 0 n
        |> exp
        |> String.fromFloat


exp : Float -> Float
exp number =
    let
        term : Int -> Float
        term n =
            number ^ toFloat n / toFloat (fat n)
    in
    List.sum <| List.map term (List.range 0 30)


fat : Int -> Int
fat n =
    if n <= 1 then
        1

    else
        n * fat (n - 1)



-- VIEW --------------------------------------------------


view : Model -> Html Msg
view m =
    div []
        [ h1 [] [ text m.out ]
        , input [ placeholder "Type here", onInput Replace ] []
        , button [ onClick (inputClick m.input) ] [ text "Exp?" ]
        ]


inputClick : String -> Msg
inputClick str =
    Send (str |> String.toFloat)
