module Piano.Msg exposing (..)

import Bytes
import File
import Piano.TrackNumber


type Msg
    = FileSelectRequested
    | FileSelected File.File
    | FileLoaded File.File Bytes.Bytes
      --
    | TrackToggleRequested Piano.TrackNumber.TrackNumber Bool
