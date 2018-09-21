module Brainfuck exposing
    ( Commands(..)
    , parseBrainFuck
    )


type Commands
    = GoRight -- >
    | GoLeft -- <
    | Increment -- +
    | Decrement -- -
    | Print -- .
    | Read -- ,
    | LoopL -- [
    | LoopR -- ]
    | Comment Char -- anything else


parseBrainFuck : String -> List Commands
parseBrainFuck code =
    let
        charToBf chr =
            case chr of
                '>' ->
                    GoRight

                '<' ->
                    GoLeft

                '+' ->
                    Increment

                '-' ->
                    Decrement

                '.' ->
                    Print

                ',' ->
                    Read

                '[' ->
                    LoopL

                ']' ->
                    LoopR

                c ->
                    Comment c
    in
    List.map charToBf (String.toList code)
