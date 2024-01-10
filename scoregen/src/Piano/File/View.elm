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
        , spacing 4
        ]
        [ row [ centerX ]
            (List.indexedMap
                (\i _ ->
                    let
                        number : Piano.TrackNumber.TrackNumber
                        number =
                            Piano.TrackNumber.fromInt i
                    in
                    Element.Input.checkbox
                        (interactive
                            [ width (px 40)
                            , height (px 40)
                            ]
                        )
                        { icon = Element.Input.defaultCheckbox
                        , label = Element.Input.labelRight [ centerY ] (text (String.fromInt (Piano.TrackNumber.toInt number + 1)))
                        , checked = not (Dict.Any.member Piano.TrackNumber.toInt number a.disabledTracks)
                        , onChange = Piano.Msg.TrackToggleRequested number
                        }
                )
                ((\( x, x2 ) -> x :: x2) a.midi.tracks)
            )
        , row [ spacing 8, centerX ]
            [ el [ width (px 128), Element.Font.alignRight ]
                (text
                    (if Piano.Scale.toInt a.scale < 0 then
                        Piano.Scale.toString a.scale

                     else
                        ""
                    )
                )
            , Element.Input.button
                (interactive
                    [ width (px 40)
                    , height (px 40)
                    , Element.Font.center
                    ]
                )
                { onPress = Just (Piano.Msg.ScaleChanged (Maybe.withDefault a.scale (Piano.Scale.fromInt (Piano.Scale.toInt a.scale - 1))))
                , label = text "♭"
                }
            , Element.Input.button
                (interactive
                    [ width (px 40)
                    , height (px 40)
                    , Element.Font.center
                    ]
                )
                { onPress = Just (Piano.Msg.ScaleChanged (Maybe.withDefault a.scale (Piano.Scale.fromInt (Piano.Scale.toInt a.scale + 1))))
                , label = text "♯"
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
                        , Element.Background.color lightBlack
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
                        , Element.Background.color lightBlack
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
                                rgba 0 0 0 0.05
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
         , Element.Background.color darkBlack
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

            1 ->
                if Piano.Scale.toInt scale > 0 then
                    indexToColor 0

                else
                    indexToColor 1

            2 ->
                indexToColor 1

            3 ->
                if Piano.Scale.toInt scale > 0 then
                    indexToColor 1

                else
                    indexToColor 2

            4 ->
                indexToColor 2

            5 ->
                if Piano.Scale.toInt scale > 0 then
                    indexToColor 2

                else
                    indexToColor 3

            6 ->
                if Piano.Scale.toInt scale > 0 then
                    indexToColor 3

                else
                    indexToColor 4

            7 ->
                indexToColor 4

            8 ->
                if Piano.Scale.toInt scale > 0 then
                    indexToColor 4

                else
                    indexToColor 5

            9 ->
                indexToColor 5

            10 ->
                if Piano.Scale.toInt scale > 0 then
                    indexToColor 5

                else
                    indexToColor 6

            11 ->
                if Piano.Scale.toInt scale == 0 then
                    indexToColor 6

                else if Piano.Scale.toInt scale > 0 then
                    indexToColor 6

                else
                    indexToColor 0

            _ ->
                indexToColor -1

    else
        indexToColor -1


indexToInterval : Int -> Int
indexToInterval a =
    case a of
        0 ->
            0

        4 ->
            1

        1 ->
            2

        5 ->
            3

        2 ->
            4

        6 ->
            5

        3 ->
            6

        _ ->
            -1


indexToColor : Int -> Color
indexToColor a =
    case indexToInterval a of
        0 ->
            rgb255 255 0 0

        1 ->
            rgb255 251 155 96

        2 ->
            rgb255 255 204 0

        3 ->
            rgb255 0 185 0

        4 ->
            rgb255 0 210 255

        5 ->
            rgb255 0 118 255

        6 ->
            rgb255 83 58 152

        _ ->
            rgb255 169 169 169



--


numberOfOctaves : Int
numberOfOctaves =
    5


noteThickness : Int
noteThickness =
    8
