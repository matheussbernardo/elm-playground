module HelloName exposing (main)

{-| Pergunta um número e diz várias coisas
sobre o número
-}

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }



-- MODEL


type alias Model =
    { input : String
    , value : Int
    }


init : Model
init =
    { input = "", value = 0 }



-- UPDATE


type Msg
    = NoOp
    | OnUpdate String
    | OnSend


update : Msg -> Model -> Model
update msg m =
    case msg of
        NoOp ->
            m

        OnUpdate st ->
            { m | input = st }

        OnSend ->
            { input = ""
            , value =
                String.toInt m.input
                    |> Maybe.withDefault 0
            }



-- VIEW


view : Model -> Html Msg
view m =
    div []
        [ div []
            [ Html.form [ onSubmit OnSend ]
                [ inputElement m
                , inputButton
                ]
            , values m.value
            ]
        ]


inputElement : Model -> Html Msg
inputElement m =
    input
        [ value m.input
        , onInput OnUpdate
        , placeholder "Digite um número"
        ]
        []


inputButton =
    input
        [ type_ "submit"
        , value "Send"
        ]
        []

values : Int -> Html Msg
values n =
    div []
        [viewCollatz n
        ]


viewCollatz : Int -> Html Msg
viewCollatz n =
    div []
        [ text "Sequencia Collatz"
        , viewList (collatzList n)
        ]


collatzList : Int -> List Int
collatzList n =
    if n <= 0 then
        []

    else
        List.reverse (collatzAcc n [n])


collatzAcc : Int -> List Int -> List Int
collatzAcc n list =
    if n == 1 then
        list

    else
        let
            n1 =
                if modBy 2 n == 0 then
                    n // 2

                else
                    3 * n + 1
        in
        collatzAcc n1 (n1 :: list)




viewList : List Int -> Html Msg
viewList lst =
    let
        makeLi n =
            li [] [ text (String.fromInt n) ]

        children =
            List.map makeLi lst
    in
    ol [] children


