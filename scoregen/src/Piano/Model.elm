module Piano.Model exposing (..)

import Dict.Any
import Midi
import Piano.File


type alias Model =
    { file : Result FileError Piano.File.File
    , activeNotes : Dict.Any.Dict Midi.Note ()
    }



--


type FileError
    = NotLoaded
    | DecodeError
