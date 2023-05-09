module Piano.Model.Update exposing (..)

import Dict.Any
import File
import File.Select
import Json.Decode
import Midi.Decode
import Piano.File
import Piano.Model
import Piano.Msg
import Piano.TrackNumber
import Task


init : Json.Decode.Value -> ( Piano.Model.Model, Cmd Piano.Msg.Msg )
init _ =
    ( Piano.Model.Model (Err Piano.Model.NotLoaded)
    , Cmd.none
    )



--


update : Piano.Msg.Msg -> Piano.Model.Model -> ( Piano.Model.Model, Cmd Piano.Msg.Msg )
update msg =
    case msg of
        Piano.Msg.FileSelectRequested ->
            \model -> ( model, File.Select.file [ "audio/midi" ] Piano.Msg.FileSelected )

        Piano.Msg.FileSelected b ->
            \model -> ( model, File.toBytes b |> Task.perform (Piano.Msg.FileLoaded b) )

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



--


subscriptions : Piano.Model.Model -> Sub Piano.Msg.Msg
subscriptions _ =
    Sub.none
