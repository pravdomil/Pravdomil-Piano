module Piano.File exposing (..)

import Dict.Any
import File
import Midi
import Piano.TrackNumber


type alias File =
    { file : File.File
    , midi : Midi.File
    , disabledTracks : Dict.Any.Dict Piano.TrackNumber.TrackNumber ()
    , scale : Int
    }
