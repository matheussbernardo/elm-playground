module DebugConsole exposing (init, main, update, view)

import Browser
import Debug exposing (..)
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
            { m | out = "Hello " ++ st }

        Replace st ->
            { m | input = st }



-- VIEW --------------------------------------------------


view : Model -> Html Msg
view m =
    h1 [] [ text  (Debug.log "number" "HUE") ]