module Piano.File.View exposing (..)

import Dict.Any
import Element exposing (..)
import Element.Background
import Element.Border
import Element.Input
import List.Extra
import Midi
import Piano.File
import Piano.Msg
import Piano.Note
import Piano.NoteType
import Piano.Scale
import Piano.TrackNumber
import Piano.Utils.Theme exposing (..)


viewFile : Dict.Any.Dict Midi.Note () -> Piano.File.File -> Element Piano.Msg.Msg
viewFile activeNotes a =
    let
        tracks : List Midi.Track
        tracks =
            List.Extra.removeIfIndex
                (\x -> Dict.Any.member Piano.TrackNumber.toInt (Piano.TrackNumber.fromInt x) a.disabledTracks)
                ((\( x, x2 ) -> x :: x2) a.midi.tracks)

        notes : List Piano.Note.Note
        notes =
            List.concatMap Piano.Note.trackToNotes tracks
    in
    column
        [ width fill
        , spacing 8
        , paddingEach (EdgesXY 0 0 8 16)
        ]
        [ el [ width (px 128), centerX ]
            (Element.Input.slider
                [ height (px 32)
                , behindContent
                    (el
                        [ width fill
                        , height (px 1)
                        , centerY
                        , Element.Background.color lightBlack
                        ]
                        none
                    )
                ]
                { onChange = \x -> Piano.Msg.ScaleChanged (Maybe.withDefault a.scale (Piano.Scale.fromInt (round x)))
                , label = Element.Input.labelLeft [ centerY, width (px 32) ] (text (Piano.Scale.toString a.scale))
                , min = -6
                , max = 6
                , step = Just 1
                , value = toFloat (Piano.Scale.toInt a.scale)
                , thumb = Element.Input.defaultThumb
                }
            )
        , viewNotes a.scale activeNotes a.midi notes
        ]


viewNotes : Piano.Scale.Scale -> Dict.Any.Dict Midi.Note () -> Midi.File -> List Piano.Note.Note -> Element msg
viewNotes scale activeNotes file notes =
    let
        width_ : Int
        width_ =
            noteThickness * 12 * numberOfOctaves

        height_ : Int
        height_ =
            32 + ceiling (List.foldl (\x acc -> max (ticksToFloat x.time + ticksToFloat x.length) acc) 0 notes)

        octaveLines : List (Element msg)
        octaveLines =
            List.map
                (\x ->
                    el
                        [ width (px 1)
                        , height (px height_)
                        , moveRight (toFloat (noteThickness * 12 * x))
                        , Element.Background.color darkGray
                        ]
                        none
                )
                (List.range 1 (numberOfOctaves - 1))

        barsSpacing : Float
        barsSpacing =
            ticksToFloat (Midi.Ticks ((\(Midi.TicksPerBeat x) -> x) file.tempo))

        bars : List (Element msg)
        bars =
            List.map
                (\x ->
                    el
                        [ width (px width_)
                        , height (px 1)
                        , moveDown (barsSpacing * toFloat x)
                        , Element.Background.color lightGray
                        ]
                        none
                )
                (List.range 1 (floor (toFloat height_ / barsSpacing)))

        activeNotes_ : List (Element msg)
        activeNotes_ =
            List.map
                (\x ->
                    el
                        [ width (px noteThickness)
                        , height (px height_)
                        , moveRight (toFloat (((\(Midi.Note x2) -> x2) x - 12 * 3) * noteThickness) - (toFloat noteThickness / 2))
                        , Element.Background.color (noteToColor x)
                        , Element.Border.shadow (shadow 4)
                        , alpha 0.5
                        ]
                        none
                )
                (Dict.Any.keys activeNotes)

        ticksToFloat : Midi.Ticks -> Float
        ticksToFloat (Midi.Ticks b) =
            toFloat b * 0.06

        viewNote : Piano.Note.Note -> Element msg
        viewNote b =
            el
                [ width (px noteThickness)
                , height (px (max 8 (round (ticksToFloat b.length))))
                , moveRight (toFloat (((\(Midi.Note x) -> x) b.note - 12 * 3) * noteThickness) - (toFloat noteThickness / 2))
                , moveDown (ticksToFloat b.time)
                , Element.Background.color (noteToColor b.note)
                , Element.Border.shadow (shadow 4)
                , Element.Border.rounded 4
                ]
                none

        scaleNotes : List (Element msg)
        scaleNotes =
            List.map
                (\x ->
                    let
                        note : Midi.Note
                        note =
                            Midi.Note x
                    in
                    el
                        [ width (px noteThickness)
                        , height fill
                        , moveRight (toFloat (((\(Midi.Note x2) -> x2) note - 12 * 3) * noteThickness) - (toFloat noteThickness / 2))
                        , Element.Background.color
                            (if Piano.Scale.isNoteInScale scale note then
                                rgba 0 0 0 0

                             else
                                rgba 0 0 0 0.1
                            )
                        ]
                        none
                )
                (List.range 0 (numberOfOctaves * 12))
    in
    el
        ([ width (px width_)
         , height (px height_)
         , centerX
         , Element.Background.color white
         , Element.Border.shadow (shadow 80)
         , Element.Border.rounded 16
         , clip
         ]
            ++ List.map inFront bars
            ++ List.map inFront octaveLines
            ++ List.map inFront scaleNotes
            ++ List.map inFront activeNotes_
            ++ List.map (viewNote >> inFront) notes
        )
        none


noteToColor : Midi.Note -> Color
noteToColor a =
    case Piano.NoteType.toInterval (Piano.NoteType.fromMidiNote a) of
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


numberOfOctaves : Int
numberOfOctaves =
    5


noteThickness : Int
noteThickness =
    8
