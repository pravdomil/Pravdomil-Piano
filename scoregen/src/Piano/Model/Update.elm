module Piano.Model.Update exposing (..)

import Dict.Any
import File
import File.Select
import JavaScript.Decoder
import Json.Decode
import Midi
import Midi.Decode
import Piano.File
import Piano.Model
import Piano.Msg
import Piano.TrackNumber
import Platform.Extra
import Task
import WebMidi


init : Json.Decode.Value -> ( Piano.Model.Model, Cmd Piano.Msg.Msg )
init _ =
    ( Piano.Model.Model
        (Err Piano.Model.NotLoaded)
        Dict.Any.empty
    , Task.attempt Piano.Msg.WebMidiInitialized WebMidi.init
    )



--


update : Piano.Msg.Msg -> Piano.Model.Model -> ( Piano.Model.Model, Cmd Piano.Msg.Msg )
update msg =
    case msg of
        Piano.Msg.FileSelectRequested ->
            \x -> ( x, File.Select.file [ "audio/midi" ] Piano.Msg.FileSelected )

        Piano.Msg.FileSelected b ->
            \model -> ( model, Task.perform (Piano.Msg.FileLoaded b) (File.toBytes b) )

        Piano.Msg.FileLoaded b c ->
            \model ->
                ( { model
                    | file =
                        Midi.Decode.file c
                            |> Result.fromMaybe Piano.Model.DecodeError
                            |> Result.map (\x -> Piano.File.File b x Dict.Any.empty)
                  }
                , Cmd.none
                )

        Piano.Msg.TrackToggleRequested b c ->
            \model ->
                ( { model
                    | file =
                        Result.map
                            (\x ->
                                { x
                                    | disabledTracks =
                                        if not c then
                                            Dict.Any.insert Piano.TrackNumber.toInt b () x.disabledTracks

                                        else
                                            Dict.Any.remove Piano.TrackNumber.toInt b x.disabledTracks
                                }
                            )
                            model.file
                  }
                , Cmd.none
                )

        Piano.Msg.WebMidiInitialized _ ->
            Platform.Extra.noOperation

        Piano.Msg.WebMidiMessageReceived a ->
            case
                Json.Decode.decodeValue JavaScript.Decoder.bytes a
                    |> Result.toMaybe
                    |> Maybe.andThen Midi.Decode.eventType
            of
                Just (Midi.NoteOn _ b _) ->
                    \x -> ( { x | activeNotes = Dict.Any.insert (\(Midi.Note x2) -> x2) b () x.activeNotes }, Cmd.none )

                Just (Midi.NoteOff _ b _) ->
                    \x -> ( { x | activeNotes = Dict.Any.remove (\(Midi.Note x2) -> x2) b x.activeNotes }, Cmd.none )

                _ ->
                    Platform.Extra.noOperation



--


subscriptions : Piano.Model.Model -> Sub Piano.Msg.Msg
subscriptions _ =
    WebMidi.webMidi Piano.Msg.WebMidiMessageReceived
