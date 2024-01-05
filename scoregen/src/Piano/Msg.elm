module Piano.Msg exposing (..)

import Bytes
import File
import JavaScript
import Json.Decode
import Piano.TrackNumber


type Msg
    = FileSelectRequested
    | FileSelected File.File
    | FileLoaded File.File Bytes.Bytes
      --
    | TrackToggleRequested Piano.TrackNumber.TrackNumber Bool
      --
    | WebMidiInitialized (Result JavaScript.Error ())
    | WebMidiMessageReceived Json.Decode.Value
