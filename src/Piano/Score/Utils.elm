module Piano.Score.Utils exposing (..)

import Dict.Any
import Midi


type alias Note =
    { channel : Midi.Channel
    , note : Midi.Note
    , time : Midi.Ticks
    , length : Midi.Ticks
    }


trackToNotes : Midi.Track -> List Note
trackToNotes a =
    let
        toComparable : ( Midi.Channel, Midi.Note ) -> ( Int, Int )
        toComparable =
            Tuple.mapBoth (\(Midi.Channel v) -> v) (\(Midi.Note v) -> v)

        fn : Midi.Event -> ( Midi.Ticks, OpenNotes, List Note ) -> ( Midi.Ticks, OpenNotes, List Note )
        fn b ( time, open, acc ) =
            let
                nextTime : Midi.Ticks
                nextTime =
                    time |> ticksPlus b.delta

                noteOn : Midi.Channel -> Midi.Note -> ( Midi.Ticks, Dict.Any.Dict ( Midi.Channel, Midi.Note ) Midi.Ticks, List Note )
                noteOn channel note =
                    ( nextTime
                    , open |> Dict.Any.insert toComparable ( channel, note ) nextTime
                    , acc
                    )

                noteOff : Midi.Channel -> Midi.Note -> ( Midi.Ticks, Dict.Any.Dict ( Midi.Channel, Midi.Note ) Midi.Ticks, List Note )
                noteOff channel note =
                    case Dict.Any.get toComparable ( channel, note ) open of
                        Just c ->
                            ( nextTime
                            , open |> Dict.Any.remove toComparable ( channel, note )
                            , acc |> (::) (Note channel note c (ticksMinus nextTime c))
                            )

                        Nothing ->
                            default

                default : ( Midi.Ticks, OpenNotes, List Note )
                default =
                    ( nextTime
                    , open
                    , acc
                    )
            in
            case b.type_ of
                Midi.NoteOff c d _ ->
                    noteOff c d

                Midi.NoteOn c d e ->
                    if e == Midi.Velocity 0 then
                        noteOff c d

                    else
                        noteOn c d

                _ ->
                    default
    in
    a
        |> List.foldl fn ( Midi.Ticks 0, Dict.Any.empty, [] )
        |> (\( _, _, v ) -> v)



--


type alias OpenNotes =
    Dict.Any.Dict ( Midi.Channel, Midi.Note ) Midi.Ticks



--


ticksPlus : Midi.Ticks -> Midi.Ticks -> Midi.Ticks
ticksPlus (Midi.Ticks a) (Midi.Ticks b) =
    Midi.Ticks (a + b)


ticksMinus : Midi.Ticks -> Midi.Ticks -> Midi.Ticks
ticksMinus (Midi.Ticks a) (Midi.Ticks b) =
    Midi.Ticks (a - b)
