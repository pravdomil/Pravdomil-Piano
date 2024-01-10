module Piano.Scale exposing (..)

import List.Extra
import Midi


type Scale
    = Base
      --
    | Sharp1
    | Sharp2
    | Sharp3
    | Sharp4
    | Sharp5
    | Sharp6
      --
    | Flat1
    | Flat2
    | Flat3
    | Flat4
    | Flat5
    | Flat6


fromInt : Int -> Maybe Scale
fromInt a =
    case a of
        0 ->
            Just Base

        1 ->
            Just Sharp1

        2 ->
            Just Sharp2

        3 ->
            Just Sharp3

        4 ->
            Just Sharp4

        5 ->
            Just Sharp5

        6 ->
            Just Sharp6

        _ ->
            case negate a of
                1 ->
                    Just Flat1

                2 ->
                    Just Flat2

                3 ->
                    Just Flat3

                4 ->
                    Just Flat4

                5 ->
                    Just Flat5

                6 ->
                    Just Flat6

                _ ->
                    Nothing


toInt : Scale -> Int
toInt a =
    case a of
        Base ->
            0

        --
        Sharp1 ->
            1

        Sharp2 ->
            2

        Sharp3 ->
            3

        Sharp4 ->
            4

        Sharp5 ->
            5

        Sharp6 ->
            6

        --
        Flat1 ->
            -1

        Flat2 ->
            -2

        Flat3 ->
            -3

        Flat4 ->
            -4

        Flat5 ->
            -5

        Flat6 ->
            -6


toString : Scale -> String
toString a =
    case a of
        Base ->
            ""

        --
        Sharp1 ->
            "♯"

        Sharp2 ->
            "♯♯"

        Sharp3 ->
            "♯♯♯"

        Sharp4 ->
            "♯♯♯♯"

        Sharp5 ->
            "♯♯♯♯♯"

        Sharp6 ->
            "♯♯♯♯♯♯"

        --
        Flat1 ->
            "♭"

        Flat2 ->
            "♭♭"

        Flat3 ->
            "♭♭♭"

        Flat4 ->
            "♭♭♭♭"

        Flat5 ->
            "♭♭♭♭♭"

        Flat6 ->
            "♭♭♭♭♭♭"


toName : Scale -> String
toName a =
    case a of
        Base ->
            "C"

        --
        Sharp1 ->
            "G"

        Sharp2 ->
            "D"

        Sharp3 ->
            "A"

        Sharp4 ->
            "E"

        Sharp5 ->
            "B"

        Sharp6 ->
            "F♯"

        --
        Flat1 ->
            "F"

        Flat2 ->
            "B♭"

        Flat3 ->
            "E♭"

        Flat4 ->
            "A♭"

        Flat5 ->
            "D♭"

        Flat6 ->
            "G♭"


toSemitones : Scale -> Int
toSemitones a =
    case a of
        Base ->
            0

        --
        Sharp1 ->
            7

        Sharp2 ->
            2

        Sharp3 ->
            9

        Sharp4 ->
            4

        Sharp5 ->
            11

        Sharp6 ->
            6

        --
        Flat1 ->
            5

        Flat2 ->
            10

        Flat3 ->
            3

        Flat4 ->
            8

        Flat5 ->
            1

        Flat6 ->
            6


isNoteInScale : Scale -> Midi.Note -> Bool
isNoteInScale scale a =
    case modBy 12 ((\(Midi.Note x) -> x) a + toSemitones scale) of
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


fromMidi : Midi.File -> Maybe Scale
fromMidi a =
    List.Extra.findMap
        (\x ->
            List.Extra.findMap
                (\x2 ->
                    case x2.type_ of
                        Midi.KeySignature x3 _ ->
                            fromInt x3

                        _ ->
                            Nothing
                )
                x
        )
        ((\( x, x2 ) -> x :: x2) a.tracks)
