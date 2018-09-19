module Json exposing
    ( Color(..)
    , Person
    , decoded
    , example
    , main
    , personDecoder
    )

import Browser
import Html exposing (..)
import Json.Decode as D
import Json.Encode as E


type alias Person =
    { name : String
    , age : Int
    , email : Maybe String
    , colors : List Color
    }


type Color
    = NamedColor String
    | HexColor RGB


type alias RGB =
    ( Int, Int, Int )


example =
    """
{ 
    "name": "John",
    "email": null,
    "foo": "bar",
    "colors": ["red", [255, 0, 0]],
    "age": 42
}
"""


rgbDecoder : D.Decoder RGB
rgbDecoder =
    D.index 0 D.int
        |> D.andThen
            (\val1 ->
                D.index 1 D.int
                    |> D.andThen
                        (\val2 ->
                            D.index 2 D.int
                                |> D.andThen (\val3 -> D.succeed ( val1, val2, val3 ))
                        )
            )


colorDecoder : D.Decoder Color
colorDecoder =
    D.oneOf
        [ D.map HexColor rgbDecoder
        , D.map NamedColor D.string
        ]


personDecoder : D.Decoder Person
personDecoder =
    D.map4 Person
        (D.field "name" D.string)
        (D.field "age" D.int)
        (D.field "email" (D.maybe D.string))
        (D.field "colors" (D.list colorDecoder))


decoded =
    D.decodeString personDecoder example


main =
    div []
        [ div []
            [ pre [] [ text example ] ]
        , div
            []
            [ text (Debug.toString decoded) ]
        ]
