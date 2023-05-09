module Piano.Score exposing (..)

import Dict.Any
import Element.PravdomilUi exposing (..)
import List.Extra
import Midi
import Piano.Model
import Piano.Score.Utils
import Piano.Utils.Theme exposing (..)


viewFile : Piano.Model.File -> Element msg
viewFile a =
    let
        tracks : List Midi.Track
        tracks =
            a.midi.tracks
                |> (\( v1, v2 ) -> v1 :: v2)
                |> List.Extra.removeIfIndex
                    (\v ->
                        Dict.Any.member Piano.Model.trackNumberToInt (Piano.Model.TrackNumber v) a.disabledTracks
                    )

        notes : List Piano.Score.Utils.Note
        notes =
            tracks |> List.concatMap Piano.Score.Utils.trackToNotes
    in
    column
        [ width fill
        , height fill
        , spacing 8
        , paddingEach 0 0 8 16
        , scrollbars
        ]
        [ viewNotes a.midi notes
        ]


viewNotes : Midi.File -> List Piano.Score.Utils.Note -> Element msg
viewNotes file notes =
    let
        numberOfOctaves : Int
        numberOfOctaves =
            6

        noteHeight : Int
        noteHeight =
            8

        width_ : Int
        width_ =
            notes
                |> List.foldl (\v acc -> max (ticksToFloat v.time + ticksToFloat v.length) acc) 0
                |> ceiling
                |> (+) 32

        height_ : Int
        height_ =
            noteHeight * 12 * numberOfOctaves

        octaveLines : List (Element msg)
        octaveLines =
            List.range 1 (numberOfOctaves - 1)
                |> List.map
                    (\v ->
                        el
                            [ width (px width_)
                            , height (px 1)
                            , moveDown (toFloat (noteHeight * 12 * v))
                            , bgColor style.black40
                            ]
                            none
                    )

        barsSpacing : Float
        barsSpacing =
            file.tempo |> (\(Midi.TicksPerBeat v2) -> v2) |> Midi.Ticks |> ticksToFloat

        bars : List (Element msg)
        bars =
            List.range 1 (ceiling (toFloat width_ / barsSpacing))
                |> List.map
                    (\v ->
                        el
                            [ width (px 1)
                            , height (px height_)
                            , moveRight (barsSpacing * toFloat v)
                            , bgColor style.black20
                            ]
                            none
                    )

        ticksToFloat : Midi.Ticks -> Float
        ticksToFloat (Midi.Ticks b) =
            toFloat b * 0.06

        viewNote : Piano.Score.Utils.Note -> Element msg
        viewNote b =
            el
                [ width (px (max 8 (round (ticksToFloat b.length))))
                , height (px noteHeight)
                , moveRight (ticksToFloat b.time)
                , moveDown (toFloat (height_ - (((\(Midi.Note v) -> v) b.note - 24) * noteHeight)) - (toFloat noteHeight / 2))
                , bgColor (noteToColor b.note)
                , borderShadow (style.shadow 4)
                , borderRounded 4
                ]
                none
    in
    el
        ([ width (px width_)
         , height (px height_)
         , bgColor style.black0
         , borderShadow (style.shadow 8)
         ]
            ++ (bars |> List.map inFront)
            ++ (octaveLines |> List.map inFront)
            ++ (notes |> List.map (viewNote >> inFront))
        )
        none


noteToColor : Midi.Note -> Color
noteToColor a =
    case a |> midiNoteToNote |> noteToInterval of
        0 ->
            rgb255 0xFF 0x00 0x00

        1 ->
            rgb255 0xFB 0x81 0x60

        2 ->
            rgb255 0xFE 0xE6 0x79

        3 ->
            rgb255 0xFF 0xCC 0x00

        4 ->
            rgb255 0x00 0x88 0x37

        5 ->
            rgb255 0x80 0xCA 0x7F

        6 ->
            rgb255 0x80 0xCF 0xE3

        7 ->
            rgb255 0x01 0xA0 0xC6

        8 ->
            rgb255 0x53 0x3A 0x98

        9 ->
            rgb255 0x99 0x83 0xBE

        10 ->
            rgb255 0xF8 0x82 0xBF

        11 ->
            rgb255 0xF0 0x04 0x7F

        _ ->
            rgb255 0x00 0x00 0x00



--


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