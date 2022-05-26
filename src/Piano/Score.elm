module Piano.Score exposing (..)

import Dict.Any
import Element.PravdomilUi exposing (..)
import List.Extra
import Midi
import Piano.Model
import Piano.Score.Utils
import Piano.UserInterface exposing (..)


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
                            , moveDown (toFloat (noteHeight * 12 * v) + (toFloat noteHeight / 2))
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
                , moveDown (toFloat (height_ - (((\(Midi.Note v) -> v) b.note - 24) * noteHeight)))
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
            rgb255 0x1A 0xAA 0xCC

        1 ->
            rgb255 0x28 0xA4 0x7F

        2 ->
            rgb255 0x4D 0xAD 0x38

        3 ->
            rgb255 0xA8 0xD7 0x29

        4 ->
            rgb255 0xFF 0xEB 0x19

        5 ->
            rgb255 0xFF 0xBF 0x0F

        6 ->
            rgb255 0xFE 0x7D 0x21

        7 ->
            rgb255 0xFC 0x3F 0x2F

        8 ->
            rgb255 0xF7 0x43 0x74

        9 ->
            rgb255 0xC5 0x32 0x96

        10 ->
            rgb255 0x81 0x4E 0xA3

        11 ->
            rgb255 0x3E 0x62 0xA4

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
            3

        Ax ->
            10

        B ->
            5

        C ->
            0

        Cx ->
            7

        D ->
            2

        Dx ->
            9

        E ->
            4

        F ->
            11

        Fx ->
            6

        G ->
            1

        Gx ->
            8
