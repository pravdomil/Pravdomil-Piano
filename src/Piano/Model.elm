module Piano.Model exposing (..)

import Piano.File


type alias Model =
    { file : Result FileError Piano.File.File
    }



--


type FileError
    = NotLoaded
    | DecodeError
