module Piano.File.View exposing (..)

import Dict.Any
import Element exposing (..)
import Element.Background
import Element.Border
import Element.Font
import Element.Input
import List.Extra
import Midi
import Piano.File
import Piano.Msg
import Piano.Note
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
        [ row [ spacing 8, centerX ]
            [ el [ width (px 128), Element.Font.alignRight ]
                (text
                    (if Piano.Scale.toInt a.scale < 0 then
                        Piano.Scale.toString a.scale

                     else
                        ""
                    )
                )
            , Element.Input.slider
                [ width (px 128)
                , height (px 32)
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
                , label = Element.Input.labelHidden "Scale"
                , min = -6
                , max = 6
                , step = Just 1
                , value = toFloat (Piano.Scale.toInt a.scale)
                , thumb = Element.Input.defaultThumb
                }
            , el [ width (px 128) ]
                (text
                    (if Piano.Scale.toInt a.scale > 0 then
                        Piano.Scale.toString a.scale

                     else
                        ""
                    )
                )
            ]
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
                        , Element.Background.color (noteToColor scale x)
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
                , Element.Background.color (noteToColor scale b.note)
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
                                rgba 0 0 0 0.2
                            )
                        ]
                        none
                )
                (List.range 36 (36 + numberOfOctaves * 12))
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


noteToColor : Piano.Scale.Scale -> Midi.Note -> Color
noteToColor scale a =
    if Piano.Scale.isNoteInScale scale a then
        case modBy 12 ((\(Midi.Note x) -> x) a) of
            0 ->
                indexToColor 0

            2 ->
                indexToColor 1

            4 ->
                indexToColor 2

            5 ->
                indexToColor 3

            7 ->
                indexToColor 4

            9 ->
                indexToColor 5

            11 ->
                indexToColor 6

            _ ->
                indexToColor -1

    else
        indexToColor -1


indexToColor : Int -> Color
indexToColor a =
    case a of
        0 ->
            rgb255 255 0 0

        1 ->
            rgb255 0 185 0

        2 ->
            rgb255 0 118 255

        3 ->
            rgb255 255 204 0

        4 ->
            rgb255 0 210 255

        5 ->
            rgb255 251 155 96

        6 ->
            rgb255 83 58 152

        _ ->
            rgb255 0 0 0



--


numberOfOctaves : Int
numberOfOctaves =
    5


noteThickness : Int
noteThickness =
    8
