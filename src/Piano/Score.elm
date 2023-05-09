module Piano.Score exposing (..)

import Dict.Any
import Element.PravdomilUi exposing (..)
import List.Extra
import Midi
import Piano.Model
import Piano.Note
import Piano.Utils.Theme exposing (..)


type Note
    = A
    | Ax
    | B
    | C
    | Cx
    | D
    | Dx
    | E
    | F
    | Fx
    | G
    | Gx


midiNoteToNote : Midi.Note -> Note
midiNoteToNote (Midi.Note a) =
    case modBy 12 a of
        1 ->
            Cx

        2 ->
            D

        3 ->
            Dx

        4 ->
            E

        5 ->
            F

        6 ->
            Fx

        7 ->
            G

        8 ->
            Gx

        9 ->
            A

        10 ->
            Ax

        11 ->
            B

        _ ->
            C


noteToInterval : Note -> Int
noteToInterval a =
    case a of
        A ->
            0

        Ax ->
            7

        B ->
            2

        C ->
            9

        Cx ->
            4

        D ->
            11

        Dx ->
            6

        E ->
            1

        F ->
            8

        Fx ->
            3

        G ->
            10

        Gx ->
            5
