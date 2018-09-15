module HelloWorld exposing (init, main, update, view)

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
    String


init : Model
init =
    "Hello World!"



-- MESSAGES ----------------------------------------------


type Msg
    = Concat String
    | Replace String


update : Msg -> Model -> Model
update msg m =
    case msg of
        Concat st ->
            st

        Replace st ->
            st



-- VIEW --------------------------------------------------


view : Model -> Html Msg
view m =
    div []
        [ h1 [] [ text m ]
        ]
