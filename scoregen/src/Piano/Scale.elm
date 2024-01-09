module Piano.Scale exposing (..)

import Midi


type Scale
    = C
      --
    | G
    | D
    | A
    | E
    | B
    | Fsharp
      --
    | F
    | Bflat
    | Eflat
    | Aflat
    | Dflat
    | Gflat


fromInt : Int -> Maybe Scale
fromInt a =
    case a of
        0 ->
            Just C

        1 ->
            Just G

        2 ->
            Just D

        3 ->
            Just A

        4 ->
            Just E

        5 ->
            Just B

        6 ->
            Just Fsharp

        _ ->
            case negate a of
                1 ->
                    Just F

                2 ->
                    Just Bflat

                3 ->
                    Just Eflat

                4 ->
                    Just Aflat

                5 ->
                    Just Dflat

                6 ->
                    Just Gflat

                _ ->
                    Nothing


toInt : Scale -> Int
toInt a =
    case a of
        C ->
            0

        --
        G ->
            1

        D ->
            2

        A ->
            3

        E ->
            4

        B ->
            5

        Fsharp ->
            6

        --
        F ->
            -1

        Bflat ->
            -2

        Eflat ->
            -3

        Aflat ->
            -4

        Dflat ->
            -5

        Gflat ->
            -6


toString : Scale -> String
toString a =
    case a of
        C ->
            "C"

        --
        G ->
            "G"

        D ->
            "D"

        A ->
            "A"

        E ->
            "E"

        B ->
            "B"

        Fsharp ->
            "F♯"

        --
        F ->
            "F"

        Bflat ->
            "B♭"

        Eflat ->
            "E♭"

        Aflat ->
            "A♭"

        Dflat ->
            "D♭"

        Gflat ->
            "G♭"


isNoteInScale : Scale -> Midi.Note -> Bool
isNoteInScale _ a =
    case modBy 12 ((\(Midi.Note x) -> x) a) of
        0 ->
            True

        2 ->
            True

        4 ->
            True

        5 ->
            True

        7 ->
            True

        9 ->
            True

        11 ->
            True

        _ ->
            False
