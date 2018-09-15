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
    { input : String, out1 : String, out2 : String }


init : Model
init =
    { input = "", out1 = "", out2 ="" }



-- MESSAGES ----------------------------------------------


type Msg
    = Calculate (List Int)
    | Replace String


update : Msg -> Model -> Model
update msg m =
    let 
        sumAndSquare n acc=
            (n ^ 2) + acc
    in

    case msg of
        Calculate list ->
            { m | out1 = List.foldl (+) 0 list |> String.fromInt, 
                  out2 = List.foldl sumAndSquare 0 list |> String.fromInt }

        Replace st ->
            { m | input = st }



-- VIEW --------------------------------------------------


view : Model -> Html Msg
view m =
    div []
        [ h1 [] [ text ("SUM LIST: " ++ m.out1) ]
        , h1 [] [ text ("SQUARE LIST: " ++ m.out2) ]
        , input [ placeholder "Type here", onInput Replace ] []
        , button [ onClick (calcClick m.input) ] [ text "Sum and Square" ]
        ]


calcClick : String -> Msg
calcClick str =
    Calculate (convertStringToList str)


convertStringToList : String -> List Int
convertStringToList str =
    let
        stringToInt s =
            String.toInt s |> Maybe.withDefault 0
    in
    str
        |> String.split ","
        |> List.map stringToInt
