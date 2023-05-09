module Piano.Model exposing (..)

import Dict.Any
import File
import Midi
import Piano.TrackNumber


type alias Model =
    { file : Result FileError File
    }



--


type alias File =
    { file : File.File
    , midi : Midi.File
    , disabledTracks : Dict.Any.Dict Piano.TrackNumber.TrackNumber ()
    }



--


type FileError
    = NotLoaded
    | DecodeError
